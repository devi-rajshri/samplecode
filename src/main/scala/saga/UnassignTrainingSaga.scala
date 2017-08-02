package saga

/* Code to show usage of actors and sagas - only meant for display and not to run*/

import akka.actor._
import akka.cluster.sharding.{ClusterSharding, ClusterShardingSettings}
import akka.util.Timeout
import saga.UnassignTrainingSaga._

import scala.concurrent.duration._
import scala.language.postfixOps

class UnassignTrainingSaga(timeoutAfter: FiniteDuration,
                           commandDispatcher: ActorSelection,
                           eventDispatcher: ActorSelection) extends Actor with ActorLogging {

  import context._

  var sagaState: UnassignTrainingSagaState = null

  override def receive: Receive = {
    case cmd: UnassignTrainings =>
      if (cmd.assignables.nonEmpty) {
        sagaState = UnassignTrainingSagaState(cmd.id, cmd.assignables.map(t => AssignableWithStatus(t)))
        sagaState.training.foreach { training =>
          commandDispatcher ! UnassignTrainingWorker.UnassignTraining(training.id, training.assignable, cmd.id)

        }
        become(awaitTrainingUnassigned(system.scheduler.scheduleOnce(timeoutAfter, self, Timeout)))
      } else {
        eventDispatcher ! NoTrainingReceivedToUnassign(cmd.id,cmd.assignables)
      }

  }

  def awaitTrainingUnassigned(timeout: Cancellable): Receive = {
    case event: UnassignTrainingWorker.TrainingUnassigned if sagaState.isNewPassed(event.assignable) =>
      val passedInstanceId = sagaState.training
        .find(_.assignable == event.assignable)
        .map(_.copy(passed = true))

      val trainings = sagaState.training.filterNot(_.assignable == event.assignable) ++ passedInstanceId
      sagaState = sagaState.copy(training = trainings)

      if (sagaState.allPassed) {
        eventDispatcher ! TrainingsAssigned(sagaState.id,
          sagaState.passedAssignables,
          sagaState.failedAssignables)

        timeout.cancel()
        stop(self)

      }
    case Timeout =>
      eventDispatcher ! TrainingsAssigned(sagaState.id,
        sagaState.passedAssignables,
        sagaState.failedAssignables)

      log.error("timeout reached waiting for creation event for WFInstanceUnassigned in WorkflowInstanceSaga")
      log.error(sagaState.toString)
      stop(self)
    case msg => log.warning("received unexpected message while waiting for WFInstanceUnassigned: {}", msg)

  }

}

object UnassignTrainingSaga {

  val shardName = "UnassignTrainingSagaShard"
  val timeoutAfter = 2 hours

  def props(commandDispatcher: ActorSelection, eventDispatcher: ActorSelection) = Props(classOf[UnassignTrainingSaga], timeoutAfter, commandDispatcher, eventDispatcher)

  def startShard(actorSystem: ActorSystem, name: String, props: Props) = {
    // we use cluster sharding to manage the creation/routing of the sagas since it is straightforward and we gain
    // the benefit of scaling beyond one node (rather than a singleton saga supervisor with one shard id)
    ClusterSharding(actorSystem).start(
      typeName = name,
      entityProps = props,
      settings = ClusterShardingSettings.create(actorSystem),
      extractEntityId = {
        case cmd: UnassignTrainingSagaCommand => (cmd.id, cmd)
        case event: UnassignTrainingSagaEvent => (event.id, event)
      },
      extractShardId = {
        case cmd: UnassignTrainingSagaCommand => (math.abs(cmd.id.hashCode) % 100).toString
        case event: UnassignTrainingSagaEvent => (math.abs(event.id.hashCode) % 100).toString
      })
  }

  case class AssignableWithStatus(assignable: Assignable, passed: Boolean = false) {
    val id = assignable match {
      case Assignable.TaskList(tlid) => tlid
      case Assignable.Workflow(wfid) => wfid
    }
  }

  case class UnassignTrainingSagaState(id: String, training: Seq[AssignableWithStatus]) {
    def passedAssignables = training.filter(_.passed).map(_.assignable)

    def failedAssignables = training.filterNot(_.passed).map(_.assignable)

    def allPassed = failedAssignables.isEmpty

    def isNewPassed(assignable: Assignable) = training.find(_.assignable == assignable).exists(_.passed == false)
  }

  trait UnassignTrainingSagaCommand {val id: String}

  trait UnassignTrainingSagaEvent{val id: String}

  case class UnassignTrainings(id: String, assignables: Seq[Assignable], initiatingUserId: Option[String] = None) extends UnassignTrainingSagaCommand



  case class TrainingsAssigned(id: String, passedInstanceIds: Seq[Assignable] =Nil,
                               failedInstanceIds: Seq[Assignable] = Nil
                               ) extends UnassignTrainingSagaEvent



  case class NoTrainingReceivedToUnassign(id: String, trainings: Seq[Assignable]) extends UnassignTrainingSagaEvent

}

sealed trait Assignable

object Assignable {

  case class TaskList(taskListInstanceId: String) extends Assignable
  case class Workflow(workflowInstanceId: String) extends Assignable

}

