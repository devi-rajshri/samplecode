import scala.util.Random

//see problem definition at bottom of class

abstract class Activity {
  def activityType: ActivityType

  def energy: Int
}

case class SleepActivity(activityType: ActivityType = Sleep("zzz zzz"), energy: Int = 10) extends Activity

case class EatActivity(activityType: ActivityType = Eat("chomp chomp"), energy: Int = 5, food: Food = Meat()) extends Activity

case class SoundActivity(activityType: ActivityType = Sound("rustle"), energy: Int = -3) extends Activity

abstract class SpecialActivity extends Activity

case class PlayActivity(activityType: ActivityType = Play("Jump up and down!!!"), energy: Int = -5) extends SpecialActivity


abstract class Animal extends AnimalActivity {
  var currentEnergy: Int = 10

  def food:List[Food]

  def name: String

  def myKind: Int

  protected def setEnergy(activity: Activity): Unit = {
    currentEnergy = currentEnergy + activity.energy
  }

  def activity(activity: Activity): String = {
    activity match {
      case _: SleepActivity => setEnergy(sleep); sleep.activityType.name
      case e: EatActivity => if(food.contains(e.food)){
        setEnergy(eat);eat.activityType.name + " food ::"+ e.food.foodType
      }else s"Cannot eat ${e.food.foodType}!"
      case _: SoundActivity => setEnergy(sound); sound.activityType.name
      case _ => ""
    }
  }

  def soundOff: String = name + " says ::" + activity(SoundActivity()) + ":: Energy Level Left" + currentEnergy

}



trait AnimalActivity {
  protected val sleep = SleepActivity()
  protected val eat = EatActivity()
  protected val sound = SoundActivity()
  protected val play = PlayActivity()
}

class Tiger extends Animal {
  val name = "Tiger"
  val food = List(Meat(), Fish(), Bugs())
  def myKind = Tiger.count
  override protected val sleep = SleepActivity(Sleep("Sleep one eye open"), 5)
  override protected val sound = SoundActivity(Sound("Roarrrrr!!!!"))
}

object Tiger {

  protected var count = 0

  def apply(): Tiger = {
    count = count + 1
    new Tiger() {
      override val name = "Tiger" + count
    }
  }

}


class Monkey extends Animal {
  val name = "Monkey"
  def myKind = Monkey.count
  val food = List(Bugs(), Grains())
  override protected val sleep = SleepActivity(Sleep("sleep on tree"))
  override protected val eat = EatActivity(Eat("snatch and eat"), 2, Grains())
  override protected val sound = SoundActivity(Sound("chatter chatter"), -4)
  override protected val play = PlayActivity(Play("Oooo Oooo Oooo"), -8)

  override def activity(activity: Activity): String = {
    activity match {
      case _:PlayActivity => if (currentEnergy + activity.energy < 1) "Monkey is too tired!!"
                            else {setEnergy(play);play.activityType.name}
      case _ => super.activity(activity)
    }


  }
}

object Monkey {

  protected var count = 0

  def apply(): Monkey = {
    count = count + 1
    new Monkey() {
      override val name = "Monkey" + count
    }
  }

}


class Snakes extends Animal {
  val name = "Snake"
  val food = List(Meat(), Fish(), Bugs())

  def myKind = Snakes.count
  override protected val sleep = SleepActivity(Sleep("sleep coiled up"))
  override protected val eat = EatActivity(activityType = Eat("swallow whole"),food = Meat())
  override protected val sound = SoundActivity(Sound("sss sss"))

}

object Snakes {
  protected var count = 0

  def apply(): Snakes = {
    count = count + 1
    new Snakes() {
      override val name = "Snake" + count
    }
  }

}

object Jungle {

  val animals = List(Tiger(), Monkey(), Tiger(), Snakes(), Monkey(), Monkey(), Monkey(), Monkey(), Monkey(), Monkey(), Snakes(), Snakes())

  def soundOff() = animals.foreach(a => println(a.soundOff))

  def doActivity() = {
    val random = new Random()
    val rand3 = random.nextInt(3)
    val rand4 = random.nextInt(4)
    val activities = List(SleepActivity(), PlayActivity(), EatActivity(), SoundActivity())
    animals.grouped(3).foreach { animalList =>
      val animal = animalList(rand3)
      println("I am " + animal.name)
      animal.activity(activities(rand4))
    }
  }

}


trait  Food {
  def foodType: String
}

case class Grains(foodType: String  = "grains") extends Food

case class Meat(foodType: String = "meat") extends Food

case class Fish(foodType: String = "fish") extends Food

case class Bugs(foodType: String = "bugs") extends Food


trait ActivityType {
  def name: String
}

case class Sleep(name: String) extends ActivityType

case class Eat(name: String) extends ActivityType

case class Sound(name: String) extends ActivityType

case class Play(name: String) extends ActivityType

/*
Create the objects that you feel would best model a jungle using your best OO design and coding practices based on the following requirements, using your language of choice.


1. The jungle contains several species of animals; tigers, monkeys and snakes.

2. All animals can do three things, make a sound, eat food, and sleep.

3. Each species of animal knows how many others of its kind exist.

4. By default when an animal’s energy level changes, it changes in the following ways:

a. -3 for making a sound

b. +5 for eating food

c. +10 for sleeping

5. The jungle can perform a sound off. This involves all of the animals in the jungle each making their sound, along with reporting their energy level.

6. Tigers get +5 energy for sleeping.

7. Monkeys get +2 energy for eating and -4 energy for making a sound.

8. Some animals have the ability to play.

9. Only monkeys can play. When they do they say "Oooo Oooo Oooo" and get -8 energy. If a monkey doesn't have enough energy to play they say "Monkey is too tired".

10. The jungle contains several types of food; meat, fish, bugs and grain.

11. Tigers can't eat grain because they have sensitive digestive systems.

12. Bonus Item: The jungle can have each animal perform a random activity out of the ones possible for that animal.
 */




