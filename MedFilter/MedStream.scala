package MedFilter
//中值滤波+排序整合成顶层
import spinal.core._
import Sort.sortTop
import Sort.SortTopCongig
import spinal.lib.slave
import utils.WaCounter
import spinal.lib.bus.amba4.axilite.AxiLite4
import spinal.lib.bus.amba4.axilite.AxiLite4SpecRenamer
import spinal.lib.Delay
import xip.Mul
import Config.MemConfig
// import MedFilter.MedFilter_V9
class Multiply extends BlackBox{
    val Config=MemConfig()
    val io=new Bundle{//component要求out有驱动，但是black box不要求out的驱动
        val CLK=in Bool()
        val A=in UInt(Config.MUL_A_IN bits)
        val B=in UInt(Config.MUL_B_In bits)
        val P=out UInt(Config.MUL_P_OUT bits)
    }
    noIoPrefix()
    mapClockDomain(clock=io.CLK)
}
class MedFilter extends Component{
    //中值滤波：卷积加排序并计算sumX和sumX2
    val Med_Filter=new MedConv
    val Sort50=new sortTop(SortTopCongig(16))
    val Config=MemConfig()
    val io=new Bundle{
        val sData=slave Stream(UInt(Config.BRAM_IN_DATA_WIDTH bits))

        val mData=out UInt(32 bits)//走stream流回到ddr
        val mValid=out Bool()
        val mReady=in Bool()
        val m_tlast=out Bool()
        val start=in Bool()//start信号控制滤波的启动和阈值计算的复位
        
        val sumX=out SInt(32 bits)
        val sumX2=out UInt(Config.MUL_P_OUT bits)
        
    }
    noIoPrefix()

    for(i<-0 to 49){
        Med_Filter.io.mData(i)<>Sort50.io.sData(i)
    }
    Med_Filter.io.mReady<>io.mReady
    Med_Filter.io.sData<>io.sData
    Med_Filter.io.start<>io.start
//sort50_data1是第二个点，0是第一个点
    
    Sort50.io.vaildIn<>Med_Filter.io.mValid
    Sort50.io.vaildOut<>io.mValid
    // (Sort50.io.mData(1)##Sort50.io.mData(0)):=
    val Med_Pixel_LowBits=Delay(Med_Filter.io.mData(12),17)
    val Med_Pixel_HighBits=Delay(Med_Filter.io.mData(37),17)

    val Low16Bits=(Med_Pixel_LowBits>Sort50.io.mData(0))?(Med_Pixel_LowBits-Sort50.io.mData(0))|B"16'b0".asUInt 
    val High16Bits=(Med_Pixel_HighBits>Sort50.io.mData(1))?(Med_Pixel_HighBits-Sort50.io.mData(1))|B"16'b0".asUInt
    io.mData:=High16Bits@@Low16Bits
    val Data_Out_Cnt=WaCounter(io.mValid&&io.mReady,log2Up((Config.COl_CNT_NUM-4)*(Config.ROW_CNT_NUM-8)),(Config.COl_CNT_NUM-4)*(Config.ROW_CNT_NUM-8)-1)//进1024*1024个点了个点了
    io.m_tlast:=Data_Out_Cnt.valid//滤波完之后图片的行列都减少8



    //阈值计算：
    val sumX=Reg(SInt(32 bits))init(0)//sumX = sumX + (tempData(i,j) - temp_med(13,1));
    val sumX2=Reg(UInt(Config.MUL_P_OUT bits))init(0)//平方和得到的值为正数
    val sumX2_LowBits=Reg(UInt(Config.MUL_P_OUT bits))init(0)
    val sumX2_HighBits=Reg(UInt(Config.MUL_P_OUT bits))init(0)
    // val Square_Sumx2_LowBits=new Multiply
    val Square_Sumx2_LowBits=Mul(16,16,32,"Signed","Signed",Config.MED_MUL_LATENCY,Config.MED_MULTYPE,this.clockDomain,"Multiply",31,0,true)
    val Square_Sumx2_HighBits=Mul(16,16,32,"Signed","Signed",Config.MED_MUL_LATENCY,Config.MED_MULTYPE,this.clockDomain,"Multiply",31,0,false)

    Square_Sumx2_LowBits.io.A:=(Med_Pixel_LowBits-Sort50.io.mData(0)).asSInt
    Square_Sumx2_LowBits.io.B:=(Med_Pixel_LowBits-Sort50.io.mData(0)).asSInt//两个无符号的数相减变成了有符号数补码

    Square_Sumx2_HighBits.io.A:=(Med_Pixel_HighBits-Sort50.io.mData(1)).asSInt
    Square_Sumx2_HighBits.io.B:=(Med_Pixel_HighBits-Sort50.io.mData(1)).asSInt


    //只做加法

    when(io.mReady&&io.mValid){
        sumX:=sumX+(Med_Pixel_LowBits-Sort50.io.mData(0)+Med_Pixel_HighBits-Sort50.io.mData(1)).asSInt//一下算两个点，所以阈值累加要累加两下
    }elsewhen(io.start&&(!RegNext(io.start))){//
        sumX:=0
    }otherwise{
        sumX:=sumX//累加阈值
    }

    when(Delay(io.mReady&&io.mValid,Config.MED_MUL_LATENCY)){
        sumX2_LowBits:=sumX2_LowBits+(Square_Sumx2_LowBits.io.P).asUInt
        sumX2_HighBits:=sumX2_HighBits+Square_Sumx2_HighBits.io.P.asUInt
    }elsewhen(io.start&&(!RegNext(io.start))){
        sumX2_LowBits:=0
        sumX2_HighBits:=0
    }otherwise{
        sumX2_LowBits:=sumX2_LowBits
        sumX2_HighBits:=sumX2_HighBits
    }

    io.sumX:=sumX//仿真得到的最后的数是对的---08、23
    io.sumX2:=sumX2_LowBits+sumX2_HighBits

}



class MedFilterStream extends Component{
    val MedFilter_Module=new MedFilter
    //中值加排序整合到一起之后，再用AXIS包一下
    val io=new Bundle{
        val m_axis_mm2s_tdata=out UInt(32 bits)
        val m_axis_mm2s_tkeep=out Bits(4 bits)
        val m_axis_mm2s_tlast=out Bool()
        val m_axis_mm2s_tready=in Bool()
        val m_axis_mm2s_tvalid=out Bool()

        val s_axis_s2mm_tdata=in UInt(32 bits)
        val s_axis_s2mm_tkeep=in Bits(4 bits)
        val s_axis_s2mm_tlast=in Bool()
        val s_axis_s2mm_tready=out Bool()
        val s_axis_s2mm_tvalid=in Bool()

        val m_tlast=out Bool()
        val start=in Bool()

        val sumX=out SInt(32 bits)
        val sumX2=out UInt(32 bits)

        
    }
    noIoPrefix()
    MedFilter_Module.io.mData<>io.m_axis_mm2s_tdata
    MedFilter_Module.io.mReady<>io.m_axis_mm2s_tready
    MedFilter_Module.io.mValid<>io.m_axis_mm2s_tvalid
    MedFilter_Module.io.m_tlast<>io.m_axis_mm2s_tlast
    io.m_axis_mm2s_tkeep:=B"4'b1111"

    MedFilter_Module.io.sData.payload<>io.s_axis_s2mm_tdata
    MedFilter_Module.io.sData.ready<>io.s_axis_s2mm_tready
    MedFilter_Module.io.sData.valid<>io.s_axis_s2mm_tvalid

    MedFilter_Module.io.m_tlast<>io.m_tlast
    MedFilter_Module.io.start<>io.start
//=============================================v9添加以下代码==============================
    io.sumX<>MedFilter_Module.io.sumX
    io.sumX2<>MedFilter_Module.io.sumX2


}
object MemGen extends App { 
    val verilog_path="./testcode_gen/MemGen" 
   //SpinalConfig(targetDirectory=verilog_path, defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH)).generateVerilog(new Med_Sort)
//    SpinalConfig(targetDirectory=verilog_path, defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH)).generateVerilog(new RegTable)
   SpinalConfig(targetDirectory=verilog_path, defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH)).generateVerilog(new MedFilterStream)
   //SpinalConfig(targetDirectory=verilog_path, defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = HIGH)).generateVerilog(new Map_Triangle_Mem)
}