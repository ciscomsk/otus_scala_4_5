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
  private def reduceOffset(offset: Int): Int = {
    // 26 - количество букв в английском алфавите
    if (offset > 26) offset % 26 else offset
  }

  def encrypt(word: String, offset: Int): String = {
    val newOffset = reduceOffset(offset)

    word
      .map(_ + newOffset)
      // ASCII code A = 65, Z = 90
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
    val newOffset = reduceOffset(offset)

    cipher
      .map(_ - newOffset)
      .map(x => if (x < 65) 91 - 65 % x else x)
      .map(_.toChar)
      .map(_.toString)
      .fold("")(_ concat _)
  }

}
