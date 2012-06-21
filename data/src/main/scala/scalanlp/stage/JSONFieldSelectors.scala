package scalanlp.stage

import generic.Mapper
import scalanlp.collection.LazyIterable
import net.liftweb.json._

// This file is copy/paste/modified from ColumnSelectors to work with
// json data instead of csv.

/**
 * Selects the given field as the identifier field for all data rows.
 * @author John Roesler
 */
case class IDField[ID: Manifest](field: String) extends Stage[LazyIterable[Item[ID, JValue]], LazyIterable[Item[String, JValue]]] {
  def setID(in: Item[ID, JValue]): Item[String, JValue] =
    in.copy(id = (in.value \ field).toString);

  override def apply(parcel: Parcel[LazyIterable[Item[ID, JValue]]]): Parcel[LazyIterable[Item[String, JValue]]] =
    Parcel(parcel.history + this, parcel.meta, parcel.data.map(setID));

  override def toString =
    "IDColumn(" + field + ")";
}

/**
 * Selects the given field from a JValue.
 *
 * @author John Roesler
 */
case class Field[ID: Manifest](field: String) extends Mapper[ID, JValue, String] {
  override def map(in: JValue): String =
    compact(render(in \ field));

  override def toString =
    "Field(" + field + ")";
}


/**
 * Selects for only the given field names.
 * @author John Roesler
 */
case class Fields[ID: Manifest](fields: String*) extends Mapper[ID, JValue, Seq[String]] {
  override def map(in: JValue) =
    fields.map(field => compact(render(in \ field)))

  override def toString =
    "Fields(" + fields.mkString(",") + ")";
}

// Don't need to implement Join here,
// since the one in ColumnSelectors works just fine using the output of Fields