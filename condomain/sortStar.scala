package xingmin.condomain
import spinal.core._
import spinal.lib._
import wa.WaCounter

object SORTEnum extends SpinalEnum(defaultEncoding = binaryOneHot) {
  val IDLE,  SORT, OUTPUT,END= newElement
}
case class SORTFsm(start: Bool) extends Area {

 // val init = Bool()
  val sortEnd = Bool()
  val outFinish = Bool()



  val currentState = Reg(SORTEnum()) init SORTEnum.IDLE
  val nextState = SORTEnum()
  currentState := nextState

  switch(currentState) {
    is(SORTEnum.IDLE){
      when(start){
        nextState := SORTEnum.SORT
      }otherwise{
        nextState := SORTEnum.IDLE
      }
    }
//
//    is(SORTEnum.INIT){
//      when(init){
//        nextState := SORTEnum.SORT
//      }otherwise{
//        nextState := SORTEnum.INIT
//      }
//    }

    is(SORTEnum.SORT){
      when(sortEnd){
        nextState := SORTEnum.OUTPUT
      }otherwise{
        nextState := SORTEnum.SORT
      }
    }


    is(SORTEnum.OUTPUT){
      when(outFinish){
        nextState := SORTEnum.END
      }otherwise{
        nextState := SORTEnum.OUTPUT
      }
    }

    is(SORTEnum.END){
        nextState := SORTEnum.IDLE
    }



  }


}




class sortStar(DATAIn_WIDTH: Int, DATAout_WIDTH: Int,DATA_WITH: Int) extends Component {
  val io = new Bundle {
    val dataIn = slave Flow (UInt(DATAIn_WIDTH bits)) //DATAIn_WIDTH    272 bit
    val connCount = in UInt (DATA_WITH bits) //连通域数量
    val start = in Bool()
    val cleanLast=in Bool()
    val last = out Bool()//给代尧
    val dmalast = out Bool()//给dma
    val mData = master Stream UInt(DATAout_WIDTH bits)
    val temp_back_std=in UInt (DATA_WITH*2 bits) //判断作比较

  }
  noIoPrefix()

  val reg1 = Vec(Reg(UInt(DATAIn_WIDTH bits)) init (0), 6)
  val fsm = SORTFsm(io.start)

 // val InitCount = WaCounter(fsm.currentState === SORTEnum.INIT, 3, 5)
  // fsm.init := InitCount.valid

  val ltycount = WaCounter(io.dataIn.fire, DATA_WITH, io.connCount - 1)
  val outputCount = WaCounter(fsm.currentState === SORTEnum.OUTPUT && io.mData.ready, 3, 5)
  val endCount = WaCounter(outputCount.valid, 3, 5)
  val endReg = Reg(Bool)



  io.last := endReg
  io.dmalast:= endCount.valid

  fsm.sortEnd := ltycount.valid
  fsm.outFinish := endCount.valid


  when(io.cleanLast) {
    endReg := False
  }

  when(io.dataIn.payload(15 downto 0) >5&&io.dataIn.payload(271 downto 236)>=io.temp_back_std) {

    when(io.dataIn.payload(271 downto 236) >= reg1(0)(271 downto 236)) {

      reg1(5) := reg1(4)
      reg1(4) := reg1(3)
      reg1(3) := reg1(2)
      reg1(2) := reg1(1)
      reg1(1) := reg1(0)
      reg1(0) := io.dataIn.payload
    } elsewhen (io.dataIn.payload(271 downto 236) >= reg1(1)(271 downto 236)) {
      reg1(5) := reg1(4)
      reg1(4) := reg1(3)
      reg1(3) := reg1(2)
      reg1(2) := reg1(1)
      reg1(1) := io.dataIn.payload
    } elsewhen (io.dataIn.payload(271 downto 236) >= reg1(2)(271 downto 236)) {
      reg1(5) := reg1(4)
      reg1(4) := reg1(3)
      reg1(3) := reg1(2)
      reg1(2) := io.dataIn.payload
    } elsewhen (io.dataIn.payload(271 downto 236) >= reg1(3)(271 downto 236)) {
      reg1(5) := reg1(4)
      reg1(4) := reg1(3)
      reg1(3) := io.dataIn.payload
    } elsewhen (io.dataIn.payload(271 downto 236) >= reg1(4)(271 downto 236)) {
      reg1(5) := reg1(4)
      reg1(4) := io.dataIn.payload
    } elsewhen (io.dataIn.payload(271 downto 236) >= reg1(5)(271 downto 236)) {
      reg1(5) := io.dataIn.payload
    }
  }





  when(fsm.currentState === SORTEnum.OUTPUT) {
    io.mData.valid := True
    endReg := endCount.valid
  } otherwise {
    io.mData.valid := False
  }

  io.mData.payload := 0
  when(fsm.currentState === SORTEnum.OUTPUT) {
  switch(endCount.count) {
    is(0) {

      io.mData.payload <> U(0, 48 bits) @@ reg1(outputCount.count)(15 downto 0)

    }
    is(1) {
      io.mData.payload <> reg1(outputCount.count)(79 downto 16)
    }
    is(2) {
      io.mData.payload <> reg1(outputCount.count)(143 downto 80)
    }
    is(3) {
      io.mData.payload <> U(0, 8 bits) @@ reg1(outputCount.count)(199 downto 144)
    }
    is(4) {
      io.mData.payload <> U(0, 28 bits) @@ reg1(outputCount.count)(235 downto 200)
    }
    is(5) {
      io.mData.payload <> U(0, 28 bits) @@ reg1(outputCount.count)(271 downto 236)
    }

  }

}
  when(fsm.currentState === SORTEnum.END) {
  for(i <-0 to 5){
      reg1(i):= 0
  }
  }



}
object sortStar extends App{
  SpinalVerilog(new sortStar(272,64,16))
 // val clkCfg = ClockDomainConfig(resetKind = SYNC,resetActiveLevel = LOW,clockEdge = RISING)
  //SpinalConfig(defaultConfigForClockDomains = clkCfg).generateVerilog(new sortStar(272,64,16))
}