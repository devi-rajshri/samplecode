import org.scalatest.FlatSpec
import org.scalatest.Matchers


class AnimalTest extends FlatSpec with Matchers {

  "Monkey" must "behave correctly" in {
    val monkey = Monkey.apply()
    monkey.myKind should be (1)
    monkey.name should be ("Monkey1")
    monkey.activity(PlayActivity()) should be ("Oooo Oooo Oooo")
    monkey.currentEnergy should be (2)
    monkey.activity(PlayActivity()) should be ("Monkey is too tired!!")
    monkey.activity(EatActivity(food=Grains())) should be ("snatch and eat food ::grains")
    monkey.currentEnergy should be (4)
    monkey.activity(EatActivity()) should be ("Cannot eat meat!")
    monkey.activity(SleepActivity()) should be ("sleep on tree")
    monkey.currentEnergy should be (14)
    monkey.activity(SoundActivity()) should be ("chatter chatter")
    monkey.currentEnergy should be (10)
    val monkey1 = Monkey.apply()
    monkey1.myKind should be (2)
    monkey1.name should be ("Monkey2")
    monkey.myKind should be (2)
  }

  "Snakes" must "behave correctly" in {
    val snake = Snakes.apply()
    snake.myKind should be (1)
    snake.name should be ("Snake1")
    snake.activity(PlayActivity()) should be ("")
    snake.currentEnergy should be (10)
    snake.activity(EatActivity()) should be ("swallow whole food ::meat")
    snake.currentEnergy should be (15)
    snake.activity(SleepActivity()) should be ("sleep coiled up")
    snake.currentEnergy should be (25)
    snake.activity(SoundActivity()) should be ("sss sss")
    snake.currentEnergy should be (22)
    val snake1 = Snakes.apply()
    snake1.myKind should be (2)
    snake1.name should be ("Snake2")
    snake.myKind should be (2)

  }

  "Tigers" must "behave correctly" in {
    val tiger = Tiger.apply()
    tiger.myKind should be (1)
    tiger.name should be ("Tiger1")
    tiger.activity(PlayActivity()) should be ("")
    tiger.currentEnergy should be (10)
    tiger.activity(EatActivity()) should be ("chomp chomp food ::meat")
    tiger.activity(EatActivity(food= Grains())) should be ("Cannot eat grains!")
    tiger.currentEnergy should be (15)
    tiger.activity(SleepActivity()) should be ("Sleep one eye open")
    tiger.currentEnergy should be (20)
    tiger.activity(SoundActivity()) should be ("Roarrrrr!!!!")
    tiger.currentEnergy should be (17)
    val tiger1 = Tiger.apply()
    tiger1.myKind should be (2)
    tiger1.name should be ("Tiger2")
    tiger.myKind should be (2)
  }
}
