package chap07

import com.cra.figaro.language.{Flip, Chain}
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.library.compound.{If, ^^}
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.util.memo

object SocialMediaMultipleInterests {
  val topics = List("sports", "politics", "movies")

  class Person() {
    def generateInterest(topic: String) = Flip(0.4)
    val interested = memo(generateInterest _)
  }

  class Post(val poster: Person) {
    val topic = Uniform(topics:_*)
    val interestMatch = Chain(topic, (s: String) => poster.interested(s))
    interestMatch.addConstraint((b: Boolean) => if (b) 1.0 else 0.2)
  }

  class Connection(person1: Person, person2: Person) {
    val connectionType = Uniform("acquaintance", "close friend", "family")
  }
  def generateConnection(pair: (Person, Person)) = new Connection(pair._1, pair._2)
  val connection = memo(generateConnection _)

  class Comment(val post: Post, val commenter: Person) {
    val interestMatch = Chain(post.topic, (s: String) => commenter.interested(s))
    val pair = ^^(interestMatch, connection(post.poster, commenter).connectionType)
    def constraint(pair: (Boolean, String)) = {
      val (interestMatch, connectionType) = pair
      if (interestMatch) 1.0
      else if (connectionType == "family") 0.8
      else if (connectionType == "close friend") 0.5
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

    post1.topic.observe("politics")
    post2.topic.observe("sports")
    post3.topic.observe("politics")

    println("Probability Brian is interested in sports = " + VariableElimination.probability(brian.interested("sports"), true))
    println("Probability Cheryl is interested in sports = " + VariableElimination.probability(cheryl.interested("sports"), true))
    println("Probability Brian is Amy's family = " + VariableElimination.probability(connection(amy, brian).connectionType, "family"))
  }
}
