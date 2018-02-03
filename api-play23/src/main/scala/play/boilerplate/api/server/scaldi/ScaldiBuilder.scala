package play.boilerplate.api.server.scaldi

import java.lang.reflect.Constructor

import play.api.{Configuration, PlayException}
import scaldi._
import scaldi.play.ControllerInjector

object ScaldiBuilder {

  object NilInj extends ImmutableInjector with ShutdownHookLifecycleManager {
    override def getBinding(identifiers: List[Identifier]): Option[Binding] = None
    override def getBindings(identifiers: List[Identifier]): List[Binding] = Nil
  }

  def loadModules(configuration: Configuration, classLoader: ClassLoader): Injector with LifecycleManager = {

    def doLoadModules(modules: Seq[Any]): Injector with LifecycleManager =
      modules.foldLeft(NilInj: Injector with LifecycleManager) {
        case (acc, inj: Injector with LifecycleManager) => acc ++ inj
        case (acc, inj: Injector) => acc ++ inj
        case (_, unknown) =>
          throw new PlayException("Unknown module type", s"Module [$unknown] is not a Scaldi module")
      }

    val (lowPrio, highPrio) = locate(configuration, classLoader).partition(m => m.isInstanceOf[ControllerInjector])

    doLoadModules(highPrio) ++ doLoadModules(lowPrio)

  }

  private def locate(configuration: Configuration, classLoader: ClassLoader): Seq[Any] = {

    val includes = configuration.getStringSeq("play.modules.enabled").getOrElse(Seq.empty)
    val excludes = configuration.getStringSeq("play.modules.disabled").getOrElse(Seq.empty)

    val moduleClassNames = includes.filterNot(excludes.contains)

    moduleClassNames.map { className =>
      constructModule(configuration, className,
        () => classLoader.loadClass(className).asInstanceOf[Class[Any]])
    }

  }

  private def constructModule[T](configuration: Configuration, className: String, loadModuleClass: () => Class[T]): T = {
    try {
      val moduleClass = loadModuleClass()

      def tryConstruct(args: AnyRef*): Option[T] = {
        val ctor: Option[Constructor[T]] = try {
          val argTypes = args.map(_.getClass)
          Some(moduleClass.getConstructor(argTypes: _*))
        } catch {
          case _: NoSuchMethodException => None
          case _: SecurityException => None
        }
        ctor.map(_.newInstance(args: _*))
      }

      {
        tryConstruct(configuration)
      } orElse {
        tryConstruct()
      } getOrElse {
        throw new PlayException("No valid constructors", "Module [" + className + "] cannot be instantiated.")
      }
    } catch {
      case e: PlayException => throw e
      case e: VirtualMachineError => throw e
      case e: ThreadDeath => throw e
      case e: Throwable => throw new PlayException(
        "Cannot load module",
        "Module [" + className + "] cannot be instantiated.",
        e)
    }
  }

}
