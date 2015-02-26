package controllers


import play.api.data.Form
import play.api.data.Forms._
import play.api.mvc._

object Application extends Controller {

  val concordanceComputer = new common.ConcreteConcordanceComputer()

  def index = Action {
    Ok(views.html.index("Concordance"))
  }


  case class FormData(essay: String)

  val inputForm = Form(
    mapping("essay" -> text)(FormData.apply)(FormData.unapply)
  )


  // Code not written with readability in mind!
  def specify = Action { implicit request =>
    import concordanceComputer.compute
    import common.WordStat

    // Formatting stats into a list of of "columns"
    def format(stats:Map[String,WordStat]): List[String] = {
      type MapEntry = (String,WordStat)
      type Comparator = (MapEntry,MapEntry) => Boolean
      val comparator:Comparator = (a,b)=> a._1 < b._1

      stats.toList.sortWith(comparator).flatMap { case (w, WordStat(count, list)) =>
          List(w, s"{$count:${list.mkString(",")}}")
      }
    }

    // Building string-based (vs. html) table given a list of columns
    def table(list: List[String], columns: Int, cellSize: Int) =
      list.grouped(columns).map { group =>
        group.map { column =>
          val formattedColumn =
            column.toString.reverse.padTo(cellSize, " ").reverse.mkString

          if (formattedColumn.length > cellSize)
            formattedColumn.take(cellSize - 3) + "..."
          else
            formattedColumn

        }.mkString(" ")
      }.mkString("\n")


    val processedForm = inputForm.bindFromRequest

    processedForm.fold(
      formWithErrors  => BadRequest,
      formData        => {
        val map = compute(formData.essay)
        val str = table(format(map), 4, 30)
        Ok(str)
      })
  }

}