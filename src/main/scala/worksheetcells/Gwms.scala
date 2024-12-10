package worksheetcells

import better.files.*
import com.esotericsoftware.kryo.io.{Input as KyroInput, Output as KryoOutput}
import com.github.benmanes.caffeine.cache.Caffeine
import com.twitter.chill.ScalaKryoInstantiator
import java.time.Instant
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import java.io.OutputStream

/** Global worksheet mutable state.
  *
  * Use this object to store state between worksheet reloads
  */
object Gwms {

  org.burningwave.core.assembler.StaticComponentContainer.Modules.exportAllToAll()

  private object store {

    val dir = File(".gwms").createDirectoryIfNotExists()
    val kryo = ScalaKryoInstantiator().setRegistrationRequired(false).newKryo()

    val cache = Caffeine.newBuilder().expireAfterAccess(10, MINUTES).build[String, (Option[Instant], Any)]()
    val cacheMap = cache.asMap.asScala

    extension (key: String) def clean = key.replace("/", "╱")

    def fileForKey(key: String): File = dir / key.clean

    def store(key: String, value: Any, deadline: Option[Instant]): Unit = {
      val f = fileForKey(key)
      if (f.exists) f.clear() else f.createFile()
      for {
        fout <- f.outputStream
        output <- KryoOutput(fout).autoClosed
      } {
        output.writeLong(deadline.map(_.toEpochMilli).getOrElse(Long.MaxValue))
        kryo.writeClassAndObject(output, value)
      }
      cacheMap(key) = deadline -> value
    }

    def load(key: String): Option[Any] = {
      val f = fileForKey(key)
      val entry = cacheMap.get(key).orElse {
        Option.when(f.exists) {
          (for {
            fis <- f.inputStream
            input <- KyroInput(fis).autoClosed
          } yield {
            val deadline = Option(input.readLong()).filter(_ != Long.MaxValue).map(Instant.ofEpochMilli)
            deadline -> kryo.readClassAndObject(input)
          }).get()
        }
      }
      val res = entry.filterNot { (deadlineOpt, value) =>
        val expired = deadlineOpt.exists(_ `isBefore` Instant.now())
        if (expired) {
          f.delete(swallowIOExceptions = true) // if expired, remove
          None
        } else {
          cacheMap(key) = (deadlineOpt, value) //ensure value is cached
        }
        expired
      }
      res.map(_._2)
    }

    def sha1(key: String): Option[String] = {
      val f = fileForKey(key)
      Option.when(f.exists) {
        (for {
          fis <- f.inputStream
        } yield {
          fis.skipNBytes(8)
          val digestStream = fis.sha1
          digestStream.transferTo(OutputStream.nullOutputStream())
          Aux.toHex(digestStream.getMessageDigest().digest())
        }).get()
      }
    }

    def remove(key: String): Unit = {
      fileForKey(key).delete(swallowIOExceptions = true)
      cacheMap.remove(key)
    }
    def readKeys(): Set[String] = dir.list.map(_.name.replace("╱", "/")).toSet

    def clearExpired(): Unit = {
      val now = Instant.now()
      dir.list
        .filter(_.inputStream.apply(fis => Instant.ofEpochMilli(fis.asObjectInputStream().readLong).isBefore(now)))
        .foreach(_.delete(swallowIOExceptions = true))
      cacheMap.filterInPlace { case (_, (deadlineOpt, _)) => !deadlineOpt.exists(_.isBefore(now)) }
    }
    def clear(): Unit = {
      dir.list.foreach(_.delete(swallowIOExceptions = true))
      cacheMap.clear()
    }
  }

  /** Store a result associated to a key for a given Duration The result must be Releasable
    */
  def store[R](key: String, duration: Duration = Duration.Inf)(f: => R): R = this.synchronized {
    store.load(key) match {
      case Some(v) => v.asInstanceOf[R]
      case _ =>
        val res = f
        store.store(key, res, Option(duration).collectFirst { case fd: FiniteDuration => Instant.now().plusNanos(fd.toNanos) })
        res
    }
  }

  /** Re-evaluates the expresion on each evaluation of the worksheet, but only keeps the latest one around, closing the previous one */
  def reeval[R](key: String, duration: Duration = Duration.Inf)(f: => R): R =
    release(key)
    store(key, duration)(f)

  /** Returns the currently associated value, without any clean up */
  def get(key: String): Option[Any] = store.load(key)

  def getSha1(key: String): Option[String] = store.sha1(key)

  def release(key: String): Unit = store.remove(key)

  def clear(): Unit = store.clear()

  def keys(): collection.Set[String] = store.readKeys()
}
