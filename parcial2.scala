object SegundoParcial {

  def main(args: Array[String]): Unit = {

    // ej.1

    def crearTransformacion(factor: Double, desplazamiento: Double): Double => Double = {
      (x: Double) => factor * x + desplazamiento
    }

    val corregirError = crearTransformacion(1.0, -1.5)
    val calibrarSensor = crearTransformacion(1.04, 0.0)
    val convertirFahrenheit = crearTransformacion(1.8, 32.0)
    
    val procesarTemperatura = corregirError andThen calibrarSensor andThen convertirFahrenheit

    println("Ejercicio 1 =")
    println(procesarTemperatura(20))
    println(procesarTemperatura(30))
    println(procesarTemperatura(10))

    // ej.2
    def crearValidador(minimo: Int)(maximo: Int)(numero: Int): Boolean = {
      numero >= minimo && numero <= maximo
    }

    val validarPorcentaje = crearValidador(0)(100)
    val validarEdadLaboral = crearValidador(18)(65)
    val validarNota = crearValidador(0)(100)

    def evaluar(numero: Int, validador: Int => Boolean): String = {
      if (validador(numero)) "valido"
      else "invalido"
    }

    println("\nEjercicio 2 =")
    println(evaluar(50, validarPorcentaje))
    println(evaluar(70, validarEdadLaboral))
    println(evaluar(34, validarEdadLaboral))
    println(evaluar(-10, validarNota))

    // ej.3

    def procesarRango(
      desde: Int,
      hasta: Int,
      transformar: Int => Int,
      combinar: (Int, Int) => Int,
      valorInicial: Int
    ): Int = {
      def recorrer(actual: Int, acumulado: Int): Int = {
        if (actual > hasta) acumulado
        else recorrer(actual + 1, combinar(acumulado, transformar(actual)))
      }
      recorrer(desde, valorInicial)
    }

    def buscar(desde: Int, hasta: Int, condicion: Int => Boolean): Int = {
      def buscarAux(n: Int): Int = {
        if (n > hasta) -1
        else if (condicion(n)) n
        else buscarAux(n + 1)
      }
      buscarAux(desde)
    }
    def cantidadDivisores(n: Int): Int = {
      (1 to n).count(d => n % d == 0)
    }

    def maxDivisores(desde: Int, hasta: Int): Int = {
      procesarRango(desde, hasta, cantidadDivisores, Math.max, 0)
    }

    def esPrimo(n: Int): Boolean = {
      if (n <= 1) false
      else (2 until n).forall(d => n % d != 0)
    }

    def primoMasBajo(desde: Int, hasta: Int): Int = {
      buscar(desde, hasta, esPrimo)
    }

    println("\nEjercicio 3 =")
    println(maxDivisores(5, 10))
    println(maxDivisores(22, 26))
    println(primoMasBajo(6, 11))
    println(primoMasBajo(24, 28))

    // ej.4

    abstract class Servicio(val nombre: String, val precioBase: Double, val duracionHoras: Int) {
      def calcularCosto(): Double
      def descripcion: String = s"$nombre ($duracionHoras horas) - Bs $precioBase"
    }

    trait ConDescuento {
      def aplicarDescuento(porcentaje: Double): Servicio
    }

    case class Catering(
      override val nombre: String,
      override val precioBase: Double,
      override val duracionHoras: Int,
      numeroPersonas: Int
    ) extends Servicio(nombre, precioBase, duracionHoras) with ConDescuento {

      override def calcularCosto(): Double = precioBase + (numeroPersonas * 25.0)

      override def aplicarDescuento(porcentaje: Double): Catering = {
        this.copy(precioBase = precioBase * (1 - porcentaje / 100.0))
      }
    }

    case class Fotografia(
      override val nombre: String,
      override val precioBase: Double,
      override val duracionHoras: Int,
      incluyeAlbum: Boolean
    ) extends Servicio(nombre, precioBase, duracionHoras) with ConDescuento {

      override def calcularCosto(): Double = {
        if (incluyeAlbum) precioBase + 350.0
        else precioBase
      }

      override def aplicarDescuento(porcentaje: Double): Fotografia = {
        this.copy(precioBase = precioBase * (1 - porcentaje / 100.0))
      }
    }

    case class Sonido(
      override val nombre: String,
      override val precioBase: Double,
      override val duracionHoras: Int,
      potenciaWatts: Int
    ) extends Servicio(nombre, precioBase, duracionHoras) {
      override def calcularCosto(): Double = precioBase + (potenciaWatts * 0.15)
    }

    object Servicio {
      def crearCatering(nombre: String, precio: Double, horas: Int, personas: Int): Option[Catering] = {
        if (precio > 0 && personas > 0) Some(Catering(nombre, precio, horas, personas))
        else None
      }

      def crearFotografia(nombre: String, precio: Double, horas: Int, album: Boolean): Option[Fotografia] = {
        if (precio > 0 && horas > 0) Some(Fotografia(nombre, precio, horas, album))
        else None
      }
    }

    case class PaqueteServicios(servicios: List[Servicio]) {
      def +(otro: Servicio): PaqueteServicios = this.copy(servicios = servicios :+ otro)

      def costoTotal: Double = servicios.map(_.calcularCosto()).sum

      override def toString: String =
        s"Paquete de ${servicios.size} servicios | Total: Bs ${costoTotal}"
    }

    println("\nEjercicio 4 =")

    val cat = Servicio.crearCatering("Catering Premium", 800.0, 5, 50).get
    val foto = Servicio.crearFotografia("Foto Profesional", 1200.0, 6, true).get

    println(cat.descripcion)
    println("Costo catering: " + cat.calcularCosto())

    val catConDesc = cat.aplicarDescuento(10)
    println("Catering con 10% de descuento: " + catConDesc.calcularCosto())

    val paquete = PaqueteServicios(List(cat)) + foto
    println(paquete)
  }
}