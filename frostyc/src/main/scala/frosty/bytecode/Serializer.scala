package frosty.bytecode

import cats.effect.IO
import cats.instances.list._
import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import java.io.{FileOutputStream, DataOutputStream}
import java.nio.charset.StandardCharsets
import frosty.algebra.DbTerm
import scala.language.higherKinds


// TODO: shouldn't there be some `Bracket` around it or something?
// (doesn't matter for now, if it starts writing, it's going to release all
// resources in a second anyway)
/** Tagless final trait that wraps imperative calls on `DataOutputStream`. */
trait DataWriter[M[_]] {
  def writeByte(b: Byte): M[Unit]
  def writeBytes(bytes: Array[Byte]): M[Unit]
  def writeInt(i: Int): M[Unit]
}

object DataWriter {
  def intoFile(file: java.io.File): IO[DataWriter[IO]] = IO {
    val out = new DataOutputStream(new FileOutputStream(file))
    new DataWriter[IO] {
      def writeByte(b: Byte): IO[Unit] = IO { out.writeByte(b) }
      def writeBytes(bytes: Array[Byte]): IO[Unit] = IO {
        out.write(bytes, 0, bytes.size)
      }
      def writeInt(i: Int): IO[Unit] = IO { out.writeInt(i) }
    }
  }
}

/** Serializes frosty's bytecode. */
object Serializer {

  def serialize[M[_]: Monad](bytecode: Bytecode)(out: DataWriter[M])
  : M[Unit] = {

    import out._
    type Ser = M[Unit]

    def label(b: Byte): Ser = out.writeByte(b)
    def writeString(s: String): Ser = {
      val bytes = s.getBytes(StandardCharsets.UTF_8)
      val len = bytes.size
      writeInt(len) >> writeBytes(bytes)
    }

    def serializeCons(c: Bc[Ser, (Int, Ser)]): Ser = c match {
      case Parallel(procs) => {
        label(format.Parallel) >> 
        writeInt(procs.size) >> 
        procs.sequence >> 
        Monad[M].unit
      }
      case Tell(c, messages) => {
        label(format.Tell) >>
        c >>
        writeInt(messages.size) >>
        messages.sequence >>
        Monad[M].unit
      }

      case Receive(c, (numVars, body)) => 
        label(format.Receive) >> c >> writeInt(numVars) >> body

      case New((numVars, body)) =>
        label(format.New) >> writeInt(numVars) >> body

      case U => label(format.Unit)
      case B(b) => label(format.Boolean) >> writeByte(if(b) 1 else 0)
      case I(i) => label(format.Integer) >> writeInt(i)
      case S(s) => label(format.String) >> writeString(s)

      case PathName(components) => {
        label(format.PathName) >>
        writeInt(components.size) >>
        components.traverse(writeString) >>
        Monad[M].unit
      }

      case BuiltInChannelName(chan) => label(chan.opcode)
      case Freeze(p) => label(format.Freeze) >> p
      case Unfreeze(p) => label(format.Unfreeze) >> p
    }

    def serializeVar(idx: Int, _ignored: Unit): Ser = {
      label(format.DeBruijnIndex) >> out.writeInt(idx)
    }

    DbTerm.foldWithIndices(bytecode)(serializeCons, serializeVar)
  }
  
}
