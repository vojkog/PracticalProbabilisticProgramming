package chap07

import com.cra.figaro.language.Flip
import com.cra.figaro.library.atomic.discrete.Uniform
import com.cra.figaro.library.compound.{If, ^^}
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.util.memo

object SocialMediaBasic {
  class Person() {
    val interest = Uniform("sports", "politics")
  }

  class Post(val poster: Person) {
    val topic = If(Flip(0.9), poster.interest, Uniform("sports", "politics"))
  }

  class Connection(person1: Person, person2: Person) {
    val connectionType = Uniform("acquaintance", "close friend", "family")
  }
  def generateConnection(pair: (Person, Person)) = new Connection(pair._1, pair._2)
  val connection = memo(generateConnection _)

  class Comment(val post: Post, val commenter: Person) {
    val topicMatch = post.topic === commenter.interest
    val pair = ^^(topicMatch, connection(post.poster, commenter).connectionType)
    def constraint(pair: (Boolean, String)) = {
      val (topicMatch, connectionType) = pair
      if (topicMatch) 1.0
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

    println("Probability Amy's interest is politics = " + VariableElimination.probability(amy.interest, "politics"))
    println("Probability Brian's interest is politics = " + VariableElimination.probability(brian.interest, "politics"))
    println("Probability Cheryl's interest is politics = " + VariableElimination.probability(cheryl.interest, "politics"))
    println("Probability Brian is Amy's family = " + VariableElimination.probability(connection(amy, brian).connectionType, "family"))
    println("Probability Cheryl is Amy's family = " + VariableElimination.probability(connection(amy, cheryl).connectionType, "family"))
    println("Probability Cheryl is Brian's family = " + VariableElimination.probability(connection(brian, cheryl).connectionType, "family"))
  }
}
