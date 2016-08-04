package chap07

import com.cra.figaro.language.Flip
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.library.compound.{If, ^^}
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.language.ElementCollection
import com.cra.figaro.language.Apply
import com.cra.figaro.util.memo

object SocialMediaTopicObjects {
  class Topic() extends ElementCollection {
    val hot = Flip(0.1)("hot", this)
  }

  val sports = new Topic()
  val politics = new Topic()

  class Person() {
    val interest = Uniform(sports, politics)
  }

  class Post(val poster: Person) extends ElementCollection {
    val topic = If(Flip(0.9), poster.interest, Uniform(sports, politics))("topic", this)
  }

  class Connection(person1: Person, person2: Person) {
    val connectionType = Uniform("acquaintance", "close friend", "family")
  }
  def generateConnection(pair: (Person, Person)) = new Connection(pair._1, pair._2)
  val connection = memo(generateConnection _)

  class Comment(val post: Post, val commenter: Person) {
    val isHot = post.get[Boolean]("topic.hot")
    val appropriateComment =
      Apply(post.topic, commenter.interest, isHot, (t1: Topic, t2: Topic, b: Boolean) => (t1 == t2) || b)
    val pair = ^^(appropriateComment, connection(post.poster, commenter).connectionType)
    def constraint(pair: (Boolean, String)) = {
      if (pair._1) 1.0
      else if (pair._2 == "family") 0.8
      else if (pair._2 == "close friend") 0.5
      else 0.1
    }
    pair.addConstraint(constraint _)
  }

  def main(args: Array[String]) {
    val amy = new Person()
    val brian = new Person()
    val cheryl = new Person()

    val post1 = new Post(amy)
    val post2 = new Post(brian)
    val post3 = new Post(amy)

    val comment1 = new Comment(post1, brian)
    val comment2 = new Comment(post1, cheryl)
    val comment3 = new Comment(post2, amy)
    val comment4 = new Comment(post3, cheryl)

    post1.topic.observe(politics)

    println("Prior probabilities")
    println("Probability Amy's interest is politics = " + VariableElimination.probability(amy.interest, politics))
    println("Probability post 2's topic is sports = " + VariableElimination.probability(post2.topic, sports))
    println("Probability post 3's topic is sports = " + VariableElimination.probability(post3.topic, sports))
    println("Probability Amy is Brian's family = " + VariableElimination.probability(connection(brian, amy).connectionType, "family"))

    sports.hot.observe(true)

    println("\nAfter observing that sports is hot")
    println("Probability Amy's interest is politics = " + VariableElimination.probability(amy.interest, politics))
    println("Probability post 2's topic is sports = " + VariableElimination.probability(post2.topic, sports))
    println("Probability post 3's topic is sports = " + VariableElimination.probability(post3.topic, sports))
    println("Probability Amy is Brian's family = " + VariableElimination.probability(connection(brian, amy).connectionType, "family"))
 }
}
