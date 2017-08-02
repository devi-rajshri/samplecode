package saga

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorSelection, ActorSystem, Cancellable, Props}
import akka.cluster.sharding.{ClusterSharding, ClusterShardingSettings}
import akka.util.Timeout
import saga.UnassignTrainingSaga._
import saga.UnassignTrainingWorker._

import scala.concurrent.duration._
import scala.language.postfixOps

/* Code to show usage of actors and sagas - only meant for display and not to run*/

class UnassignTrainingWorker(timeoutAfter: FiniteDuration,
                                 commandDispatcher: ActorSelection,
                                 eventDispatcher: ActorSelection) extends Actor with ActorLogging {

  import context._


  override def receive: Receive = {
    case cmd: UnassignTraining => {
      cmd.assignable match {
        case Assignable.Workflow(wfid)=>  // do commands and wait for events
        case Assignable.TaskList(tlid) => //commandDispatcher ! UnassignTaskListInstance(cmd.id, cmd.initiatingUserId)
      }
      become(awaitTrainingUnassigned(system.scheduler.scheduleOnce(timeoutAfter, self, Timeout),cmd.parentSagaId))
    }

  }

  def awaitTrainingUnassigned(timeout: Cancellable, parentSagaId:String): Receive = {
    case event: WorkflowInstanceUnassigned =>

      eventDispatcher ! TrainingUnassigned(parentSagaId, Assignable.Workflow(event.id))


      timeout.cancel()
      stop(self)

    case event: TaskListInstanceUnassigned =>
      eventDispatcher ! TrainingUnassigned(parentSagaId, Assignable.TaskList(event.id))
      timeout.cancel()
      stop(self)

    case Timeout =>
      log.error("timeout reached waiting for WorkflowInstanceUnassigned or TaskListInstanceUnassigned in UnassignTrainingWorkerSaga")
      stop(self)
    case msg => log.warning("received unexpected message while waiting for WorkflowInstanceUnassigned or TaskListInstanceUnassigned: {}", msg)

  }

}

object UnassignTrainingWorker {

  val shardName = "UnassignTrainingWorkerSagaShard"
  val timeoutAfter = 2 hours

  def props(commandDispatcher: ActorSelection, eventDispatcher: ActorSelection) = Props(classOf[UnassignTrainingWorker], timeoutAfter, commandDispatcher, eventDispatcher)

  def startShard(actorSystem: ActorSystem, name: String, props: Props) = {
    // we use cluster sharding to manage the creation/routing of the sagas since it is straightforward and we gain
    // the benefit of scaling beyond one node (rather than a singleton saga supervisor with one shard id)
    ClusterSharding(actorSystem).start(
      typeName = name,
      entityProps = props,
      settings = ClusterShardingSettings.create(actorSystem),
      extractEntityId = {
        case cmd: UnassignTrainingWorkerSagaCommand => (cmd.id, cmd)
        case event: UnassignTrainingWorkerSagaEvent => (event.id, event)
      },
      extractShardId = {
        case cmd: UnassignTrainingWorkerSagaCommand => (math.abs(cmd.id.hashCode) % 100).toString
        case event: UnassignTrainingWorkerSagaEvent => (math.abs(event.id.hashCode) % 100).toString
      })
  }


  trait UnassignTrainingWorkerSagaCommand {
    val id:String
  }

  trait UnassignTrainingWorkerSagaEvent{
    val id:String
  }

  case class TaskListInstanceUnassigned(id:String, assignable: Assignable)
  case class WorkflowInstanceUnassigned(id:String, assignable: Assignable)

  case class UnassignTraining(id: String, assignable: Assignable, parentSagaId:String) extends UnassignTrainingWorkerSagaCommand

  case class TrainingUnassigned(id: String, assignable: Assignable) extends UnassignTrainingWorkerSagaEvent

}

