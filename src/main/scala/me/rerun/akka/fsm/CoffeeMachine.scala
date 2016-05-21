package me.rerun.akka.fsm

import akka.actor.FSM
import CoffeeMachine._
import CoffeeProtocol._
import org.slf4j.LoggerFactory

/**
 * Created by Arun on 12/25/15.
 */

object CoffeeMachine {

  sealed trait MachineState
  case object Open extends MachineState
  case object ReadyToBuy extends MachineState
  case object PoweredOff extends MachineState

  case class MachineData(currentTxTotal: Int, costOfCoffee: Int, coffeesLeft: Int)

}

object CoffeeProtocol {

  trait UserInteraction
  trait VendorInteraction

  case class Deposit(value: Int) extends UserInteraction
  case class Balance(value: Int) extends UserInteraction
  case object Cancel extends UserInteraction
  case object BrewCoffee extends UserInteraction
  case object GetCostOfCoffee extends UserInteraction

  case object ShutDownMachine extends VendorInteraction
  case object StartUpMachine extends VendorInteraction
  case class SetNumberOfCoffee(quantity: Int) extends VendorInteraction
  case class SetCostOfCoffee(price: Int) extends VendorInteraction
  case object GetNumberOfCoffee extends VendorInteraction

  case class MachineError(errorMsg:String)

}

class CoffeeMachine extends FSM[MachineState, MachineData] {

  private val logger = LoggerFactory.getLogger(classOf[CoffeeMachine])

  startWith(Open, MachineData(currentTxTotal = 0, costOfCoffee =  5, coffeesLeft = 10))

  when(Open) {
    case Event(_,MachineData(_,_,coffeesLeft)) if (coffeesLeft<=0) =>{
      logger.warn("No more coffee")
      sender ! MachineError("No more coffee")
      goto(PoweredOff)
    }
    case Event(deposit: Deposit, MachineData(currentTxTotal, costOfCoffee, coffeesLeft)) if (deposit.value + currentTxTotal) >= stateData.costOfCoffee => {
      goto(ReadyToBuy) using stateData.copy(currentTxTotal = currentTxTotal + deposit.value)
    }
    //If the total deposit is less than than the price of the coffee, then stay in the current state with the current deposit amount incremented.
    case Event(deposit: Deposit, MachineData(currentTxTotal, costOfCoffee, coffeesLeft)) => {
      val cumulativeValue = currentTxTotal + deposit.value
      logger.debug(s"staying at open with currentTxTotal $cumulativeValue")
      stay using stateData.copy(currentTxTotal = cumulativeValue)
    }
    case Event(SetNumberOfCoffee(quantity), _) => stay using stateData.copy(coffeesLeft = quantity)
    case Event(GetNumberOfCoffee, _) => sender ! (stateData.coffeesLeft); stay()
    case Event(SetCostOfCoffee(price), _) => stay using stateData.copy(costOfCoffee = price)
    case Event(GetCostOfCoffee, _) => sender ! (stateData.costOfCoffee); stay()
  }

  when(ReadyToBuy) {
    case Event(deposit: Deposit, MachineData(currentTxTotal, costOfCoffee, coffeesLeft)) => stay() //Wait for BrewCoffee to be called
    case Event(BrewCoffee, MachineData(currentTxTotal, costOfCoffee, coffeesLeft)) => {
      val balanceToBeDispensed = currentTxTotal - costOfCoffee
      logger.debug(s"Balance is $balanceToBeDispensed")
      if (balanceToBeDispensed > 0) {
        sender ! Balance(value = balanceToBeDispensed)
        goto(Open) using stateData.copy(currentTxTotal = 0, coffeesLeft = coffeesLeft - 1)
      }
      else goto(Open) using stateData.copy(currentTxTotal = 0, coffeesLeft = coffeesLeft - 1)
    }
  }

  when(PoweredOff) {
    case (Event(StartUpMachine, _)) => goto(Open)
    case _ => {
      logger.warn("Machine Powered down.  Please start machine first with StartUpMachine")
      sender ! MachineError("Machine Powered down.  Please start machine first with StartUpMachine")
      stay()
    }
  }

  whenUnhandled {
    case Event(ShutDownMachine, MachineData(currentTxTotal, costOfCoffee, coffeesLeft)) => {
      sender ! Balance(value = currentTxTotal)
      goto(PoweredOff) using stateData.copy(currentTxTotal = 0)
    }
    case Event(Cancel, MachineData(currentTxTotal, _, _)) => {
      logger.debug(s"Balance is $currentTxTotal")
      sender ! Balance(value = currentTxTotal)
      goto(Open) using stateData.copy(currentTxTotal = 0)
    }
  }

  onTransition {
    case Open -> ReadyToBuy => logger.debug("From Transacting to ReadyToBuy")
    case ReadyToBuy -> Open => logger.debug("From ReadyToBuy to Open")
  }


}
