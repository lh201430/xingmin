package xingmin.condomain

import spinal.core._
import spinal.lib._

case class ConnDomainConfig1(DATA_WIDTH: Int, FEATURE_W: Int, FEATURE_H: Int, ROW_MEM_DEPTH: Int) { //32 11(2040) 2040 用来缓存数据（2040）
  val STREAM_DATA_WIDTH = DATA_WIDTH * 4
}

class LtyLh_Stream (connDomainConfig1: ConnDomainConfig1) extends Component{
  val io = new Bundle {
    def DATA_IN_WIDTH=64
    val m_axis_mm2s_tdata=out UInt(DATA_IN_WIDTH bits)
    val m_axis_mm2s_tkeep=out Bits(DATA_IN_WIDTH/8 bits)
    val m_axis_mm2s_tlast=out Bool()
    val m_axis_mm2s_tready=in Bool()
    val m_axis_mm2s_tvalid=out Bool()
    def DATA_OUT_WIDTH=64
    val s_axis_s2mm_tdata=in UInt(DATA_OUT_WIDTH bits)
    val s_axis_s2mm_tkeep=in Bits(DATA_OUT_WIDTH/8 bits)
    val s_axis_s2mm_tlast=in Bool()
    val s_axis_s2mm_tready=out Bool()
    val s_axis_s2mm_tvalid=in Bool()


    val start = in Bool()

    val threshold = in UInt (connDomainConfig1.DATA_WIDTH bits) //阈值s
    val tempBackMean = in UInt (connDomainConfig1.DATA_WIDTH bits)
    val signBit= in Bool()
    val temp_back_std = in UInt (connDomainConfig1.DATA_WIDTH*2 bits) //排序判断作比较
    //val sData = slave Stream UInt(connDomainConfig1.STREAM_DATA_WIDTH bits)
    //val mData = master Stream UInt(64 bits)
    //val connCount = out UInt (connDomainConfig1.DATA_WIDTH bits)
    //val last= out Bool()
    val Lty_Extracted = out Bool()



  }
  noIoPrefix()
  io.m_axis_mm2s_tkeep.setAll()


  val connDong = new connDomain(ConnDomainConfig(16, 1192, 1192, 2040))
  connDong.io.threshold<>io.threshold
  connDong.io.tempBackMean<>io.tempBackMean
  connDong.io.signBit<>io.signBit
  connDong.io.start<>io.start
  connDong.io.sData.payload:=io.s_axis_s2mm_tdata
  connDong.io.sData.valid:=io.s_axis_s2mm_tvalid
  connDong.io.sData.ready<>io.s_axis_s2mm_tready





    val sortstar = new sortStar(272,64,16)
    sortstar.io.dataIn.payload:=connDong.io.mData.payload
    sortstar.io.dataIn.valid:=connDong.io.mData.valid
    sortstar.io.connCount<>connDong.io.connCount
    sortstar.io.start<>connDong.io.dy_finish//进完所有数据
    sortstar.io.temp_back_std<>io.temp_back_std
  sortstar.io.mData.payload<>io.m_axis_mm2s_tdata
  sortstar.io.mData.valid<>io.m_axis_mm2s_tvalid
  sortstar.io.mData.ready<>io.m_axis_mm2s_tready

  sortstar.io.dmalast<>io.m_axis_mm2s_tlast
  sortstar.io.last<>io.Lty_Extracted//////给代尧
  sortstar.io.cleanLast<>io.start


}




object LtyLh_Stream extends App {
  SpinalVerilog(new LtyLh_Stream(ConnDomainConfig1(16, 1192, 1192, 2040))).printPruned()
}