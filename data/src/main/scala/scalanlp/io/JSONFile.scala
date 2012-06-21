package scalanlp.io

import scalanlp.collection.LazyIterable
import net.liftweb.json._
import scalanlp.serialization.TextSerialization
import java.io.File

// need these implicits

import io.Source
import scalanlp.pipes.Pipes

/*
 * JSONFile handles reading and writing files in JSON format.
 * Correctly parsing JSON is handled by the lift-json library
 * @author John Roesler
 */
class JSONFile(path: String) extends File(path) with LazyIterable[JValue] {

  def read(): JValue = {
    lazy val reader = Source.fromFile(path)
    val result = parse(reader.getLines().mkString("\n"))
    reader.close()
    result
  }

  /**
   * If the file contains an array, return an iterator over the elements.
   * Otherwise, return an iterator over the single JSON value in the file.
   * This is probably an object, but we'll be generous and let you parse
   * files containing anything that can be interpreted as JSON.
   * @return an iterator over the JSON in the file
   * @author John Roesler
   */
  override def iterator = {
    // Yes, we are reading in the whole file first.
    // If this turns out to be a pain (memory), we can stream over the file,
    // but we'll have to implement our own top level JSON parsing
    // to detect the elements in the array to pass to the json parser.
    val jValue = read()
    jValue match {
      case JArray(arr: List[JValue]) =>
        arr.iterator
      case _ =>
        List(jValue).iterator
    }
  }

  /**
   * @author John Roesler
   * @return a rep of the file itself, not the contents.
   */
  override def toString =
    "JSONFile(" + TextSerialization.toString(path) + ")";
}

object JSONFile {
  /* A JSON file in the current folder */
  def apply(name: String)(implicit pipes: Pipes = Pipes.global) =
    new JSONFile(pipes.file(name).getPath);

  /**JSONFile that points to a file within the given base folder. */
  def apply(base: File, name: String) =
    new JSONFile(new File(base, name).getPath);

  /**Calls file.asParcel. */
  implicit def JSONFileAsParcel(file: JSONFile) =
    scalanlp.stage.Parcel(
      history = scalanlp.stage.History.Origin(file.toString),
      meta = scalanlp.collection.immutable.DHMap() + (file: File),
      data = file.zipWithIndex.map(tup => scalanlp.stage.Item(tup._2, tup._1)));
}
