package u05lab.ex2

trait ConferenceReviewing:
  import ConferenceReviewing.*
  def loadReview(article: Int, scores: Map[Question, Int]): Unit
  def loadReview(article: Int, relevance: Int, significance: Int,
                 confidence: Int, fin: Int): Unit
  def orderedScores(article: Int, question: Question): List[Int]
  def averageFinalScore(article: Int): Double
  def acceptedArticles(): Set[Int]
  def sortedAcceptedArticles(): List[(Int, Double)]
  def averageWeightedFinalScoreMap(): Map[Int, Double]

object ConferenceReviewing:
  def apply(): ConferenceReviewing = new ConferenceReviewingImpl

  enum Question:
    case Relevance()
    case Significance()
    case Confidence()
    case Final()

class ConferenceReviewingImpl extends ConferenceReviewing:
  import u05lab.ex2.ConferenceReviewing.*
  private var reviews: List[(Int, Map[Question, Int])] = List()

  override def loadReview(article: Int, scores: Map[Question, Int]): Unit =
    this.reviews = this.reviews.appended(article, scores)

  override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
    this.reviews = this.reviews.appended(article, Map(Question.Relevance() -> relevance,
      Question.Significance() -> significance, Question.Confidence() -> confidence, Question.Final() -> fin))

  override def orderedScores(article: Int, question: Question): List[Int] =
    orderedScores(this.getEvaluetions(article), question).sorted

  private def orderedScores(l : List[Map[Question, Int]], question: Question): List[Int] = l match
      case h :: t => h(question) :: orderedScores(t, question)
      case _ => Nil

  override def averageFinalScore(article: Int): Double = this.averageScore(article, Question.Final())

  override def acceptedArticles(): Set[Int] = this.sortedAcceptedArticles().map(a => a._1).toSet

  override def sortedAcceptedArticles(): List[(Int, Double)] =
    this.getArticles.filter(a => this.averageFinalScore(a) > 5
        && this.getMaxEvaluation(a, Question.Relevance()) >= 8)
      .map(r => (r, this.averageFinalScore(r))).sortBy(x => x._2)

  override def averageWeightedFinalScoreMap(): Map[Int, Double] =
    this.getArticles.map(a => (a, this.reviews
      .filter((i, _) => i == a)
      .map((_, m) => m(Question.Confidence()).toDouble * m(Question.Final()) / 10)
      .sum / this.getEvaluetions(a).length)).toMap

  private def getArticles: List[Int] = this.reviews.map(e => e._1).distinct

  private def getMaxEvaluation(article: Int, question: Question) =
    this.getEvaluetions(article).map(q => q(question)).max

  private def getEvaluetions(article: Int): List[Map[Question, Int]] =
    this.reviews.filter((k, _) => k == article).map((_, v) => v)

  private def averageScore(article: Int, question: Question): Double =
    this.getEvaluetions(article).flatMap(m => m.get(question)).sum.toDouble / this.getEvaluetions(article).length