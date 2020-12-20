package homeworks.futures

import homeworks.HomeworksUtils.TaskSyntax

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object task_futures_sequence {

  /**
   * В данном задании Вам предлагается реализовать функцию fullSequence,
   * похожую на Future.sequence, но в отличии от нее,
   * возвращающую все успешные и не успешные результаты.
   * Возвращаемое тип функции - кортеж из двух списков,
   * в левом хранятся результаты успешных выполнений,
   * в правово результаты неуспешных выполнений
   */
  /**
   * @param futures список асинхронных задач
   * @return асинхронную задачу с кортежом из двух списков
   */
  def fullSequence[A](futures: List[Future[A]]): Future[(List[A], List[Throwable])] =
    futures.foldLeft(Future.successful(List.empty[A], List.empty[Throwable]))
    { (acc, future) =>
      acc.flatMap { case (resList, exList) =>
        future
          .map { success => (resList :+ success, exList)}
          .recover { case exception: Throwable => (resList, exList :+ exception) }
      }
    }

}
