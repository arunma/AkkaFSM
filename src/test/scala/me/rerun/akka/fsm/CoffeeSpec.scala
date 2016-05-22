package me.rerun.akka.fsm

import akka.actor.FSM.{CurrentState, SubscribeTransitionCallBack, Transition}
import akka.actor.{ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import me.rerun.akka.fsm.CoffeeMachine._
import me.rerun.akka.fsm.CoffeeProtocol._
import org.scalatest.{MustMatchers, FunSpecLike}

/**
 * Created by Arun on 12/25/15.
 */
class CoffeeSpec extends TestKit(ActorSystem("coffee-system")) with MustMatchers with FunSpecLike with ImplicitSender {

  describe("The Coffee Machine") {

    it("should allow setting and getting of price of coffee") {
      val coffeeMachine = TestActorRef(Props(new CoffeeMachine()))
      coffeeMachine ! SetCostOfCoffee(7)
      coffeeMachine ! GetCostOfCoffee
      expectMsg(7)
    }

    it("should allow setting and getting of maximum number of coffees") {
      val coffeeMachine = TestActorRef(Props(new CoffeeMachine()))
      coffeeMachine ! SetNumberOfCoffee(10)
      coffeeMachine ! GetNumberOfCoffee
      expectMsg(10)
    }


    it("should stay at Transacting when the Deposit is less then the price of the coffee") {
      val coffeeMachine = TestActorRef(Props(new CoffeeMachine()))
      coffeeMachine ! SetCostOfCoffee(5)
      coffeeMachine ! SetNumberOfCoffee(10)
      coffeeMachine ! SubscribeTransitionCallBack(testActor)

      expectMsg(CurrentState(coffeeMachine, Open))

      coffeeMachine ! Deposit(2)

      coffeeMachine ! GetNumberOfCoffee

      expectMsg(10)
    }

    it("should transition to ReadyToBuy and then Open when the Deposit is equal to the price of the coffee") {
      val coffeeMachine = TestActorRef(Props(new CoffeeMachine()))
      coffeeMachine ! SetCostOfCoffee(5)
      coffeeMachine ! SetNumberOfCoffee(10)
      coffeeMachine ! SubscribeTransitionCallBack(testActor)

      expectMsg(CurrentState(coffeeMachine, Open))

      coffeeMachine ! Deposit(5)

      expectMsg(Transition(coffeeMachine, Open, ReadyToBuy))

      coffeeMachine ! BrewCoffee
      expectMsg(Transition(coffeeMachine, ReadyToBuy, Open))

      coffeeMachine ! GetNumberOfCoffee

      expectMsg(9)
    }

    it("should transition to ReadyToBuy and then Open when the Deposit is greater than the price of the coffee") {
      val coffeeMachine = TestActorRef(Props(new CoffeeMachine()))
      coffeeMachine ! SetCostOfCoffee(5)
      coffeeMachine ! SetNumberOfCoffee(10)
      coffeeMachine ! SubscribeTransitionCallBack(testActor)

      expectMsg(CurrentState(coffeeMachine, Open))

      coffeeMachine ! Deposit(2)
      coffeeMachine ! Deposit(2)
      coffeeMachine ! Deposit(2)

      expectMsg(Transition(coffeeMachine, Open, ReadyToBuy))

      coffeeMachine ! BrewCoffee

      expectMsgPF(){
        case Balance(value)=>value==1
      }

      expectMsg(Transition(coffeeMachine, ReadyToBuy, Open))

      coffeeMachine ! GetNumberOfCoffee

      expectMsg(9)
    }

    it("should transition to Open after flushing out all the deposit when the coffee is canceled") {
      val coffeeMachine = TestActorRef(Props(new CoffeeMachine()))
      coffeeMachine ! SetCostOfCoffee(5)
      coffeeMachine ! SetNumberOfCoffee(10)
      coffeeMachine ! SubscribeTransitionCallBack(testActor)

      expectMsg(CurrentState(coffeeMachine, Open))

      coffeeMachine ! Deposit(2)
      coffeeMachine ! Deposit(2)
      coffeeMachine ! Deposit(2)

      expectMsg(Transition(coffeeMachine, Open, ReadyToBuy))

      coffeeMachine ! Cancel

      expectMsgPF(){
        case Balance(value)=>value==6
      }

      expectMsg(Transition(coffeeMachine, ReadyToBuy, Open))

      coffeeMachine ! GetNumberOfCoffee

      expectMsg(10)
    }


    it("should transition to PoweredOff state if the machine is shut down from ReadyToBuyState") {
      val coffeeMachine = TestActorRef(Props(new CoffeeMachine()))
      coffeeMachine ! SetCostOfCoffee(5)
      coffeeMachine ! SetNumberOfCoffee(10)
      coffeeMachine ! SubscribeTransitionCallBack(testActor)

      expectMsg(CurrentState(coffeeMachine, Open))

      coffeeMachine ! Deposit(2)
      coffeeMachine ! Deposit(2)
      coffeeMachine ! Deposit(2)

      expectMsg(Transition(coffeeMachine, Open, ReadyToBuy))

      coffeeMachine ! ShutDownMachine

      expectMsgPF(){
        case Balance(value)=>value==6
      }

      expectMsg(Transition(coffeeMachine, ReadyToBuy, PoweredOff))

      coffeeMachine ! GetNumberOfCoffee
      expectMsg(MachineError("Machine Powered down.  Please start machine first with StartUpMachine"))
    }

    it("should transition to PoweredOff state if the machine is shut down from Open State") {
      val coffeeMachine = TestActorRef(Props(new CoffeeMachine()))
      coffeeMachine ! SetCostOfCoffee(5)
      coffeeMachine ! SetNumberOfCoffee(10)
      coffeeMachine ! SubscribeTransitionCallBack(testActor)

      expectMsg(CurrentState(coffeeMachine, Open))

      coffeeMachine ! Deposit(2)
      coffeeMachine ! Deposit(2)

      coffeeMachine ! ShutDownMachine

      expectMsgPF(){
        case Balance(value)=>value==4
      }

      expectMsg(Transition(coffeeMachine, Open, PoweredOff))

      coffeeMachine ! GetNumberOfCoffee
      expectMsg(MachineError("Machine Powered down.  Please start machine first with StartUpMachine"))
    }


    it("should open the machine to operation if powered on") {
      val coffeeMachine = TestActorRef(Props(new CoffeeMachine()))
      coffeeMachine ! SetCostOfCoffee(5)
      coffeeMachine ! SetNumberOfCoffee(10)
      coffeeMachine ! SubscribeTransitionCallBack(testActor)

      expectMsg(CurrentState(coffeeMachine, Open))

      coffeeMachine ! Deposit(2)
      coffeeMachine ! Deposit(2)

      coffeeMachine ! ShutDownMachine

      expectMsgPF(){
        case Balance(value)=>value==4
      }

      expectMsg(Transition(coffeeMachine, Open, PoweredOff))

      coffeeMachine ! GetNumberOfCoffee
      expectMsg(MachineError("Machine Powered down.  Please start machine first with StartUpMachine"))

      coffeeMachine ! BrewCoffee
      expectMsg(MachineError("Machine Powered down.  Please start machine first with StartUpMachine"))

      coffeeMachine ! StartUpMachine

      expectMsg(Transition(coffeeMachine, PoweredOff,Open))
    }


    it("should power down if there is no coffee in the first place") {
      val coffeeMachine = TestActorRef(Props(new CoffeeMachine()))
      coffeeMachine ! SetCostOfCoffee(5)
      coffeeMachine ! SetNumberOfCoffee(0)
      coffeeMachine ! GetNumberOfCoffee
      expectMsg(MachineError("No more coffee"))
    }

    it("should power down if there is no coffee left.") {
      val coffeeMachine = TestActorRef(Props(new CoffeeMachine()))
      coffeeMachine ! SetCostOfCoffee(5)
      coffeeMachine ! SetNumberOfCoffee(5)

      (1 to 5).foreach{count=>
        println (count)
        coffeeMachine ! Deposit(5)
        coffeeMachine ! BrewCoffee
      }

      coffeeMachine ! GetNumberOfCoffee
      expectMsg(MachineError("No more coffee"))
    }

  }





}
