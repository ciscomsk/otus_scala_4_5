package homeworks.collections

import homeworks.HomeworksUtils.TaskSyntax

object task_caesar {

  /**
   * В данном задании Вам предлагается реализовать функции,
   * реализующие кодирование/декодирование строки шифром Цезаря.
   * https://ru.wikipedia.org/wiki/Шифр_Цезаря
   * Алфавит - прописные латинские буквы от A до Z.
   * Сдвиг   - неотрицательное целое число.
   * Пример: при сдвиге 2 слово "SCALA" шифруется как "UECNC".
   */
  /**
   * @param word   входное слово, которое необходимо зашифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return зашифрованное слово
   */

  // 26 - количество букв в английском алфавите
  private def recalcOffset(offset: Int): Int = {
    val posOffset = if (offset < 0) offset % 26 + 26 else offset
    val finalOffset = if (posOffset > 26) posOffset % 26 else posOffset

    finalOffset
  }

  // ASCII code A = 65, Z = 90
  def encrypt(word: String, offset: Int): String = {
    val newOffset = recalcOffset(offset)

    word
      .map(_ + newOffset)
      .map(x => if (x > 90) 64 + x % 90 else x)
      .map(_.toChar)
      .map(_.toString)
      .fold("")(_ concat _)
  }

  /**
   * @param cipher шифр, который необходимо расшифровать
   * @param offset сдвиг вперёд по алфавиту
   * @return расшифрованное слово
   */
  def decrypt(cipher: String, offset: Int): String = {
    val newOffset = recalcOffset(offset)

    cipher
      .map(_ - newOffset)
      .map(x => if (x < 65) 91 - 65 % x else x)
      .map(_.toChar)
      .map(_.toString)
      .fold("")(_ concat _)
  }

}
