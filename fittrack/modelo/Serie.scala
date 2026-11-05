package fittrack.modelo

import fittrack.util.{Resultado, Exito, Fallo}

case class Medidas(repeticiones: Int, peso: Double, segundos: Int, kilometros: Double)

object Medidas {
    def conPeso(repeticiones: Int, peso: Double): Medidas = Medidas(repeticiones, peso, 0, 0.0)
    def repeticiones(cantidad: Int): Medidas = Medidas(cantidad, 0.0, 0, 0.0)
    def tiempo(segundos: Int): Medidas = Medidas(0, 0.0, segundos, 0.0)
    def distancia(kilometros: Double): Medidas = Medidas(0, 0.0, 0, kilometros)
    def distanciaYTiempo(kilometros: Double, segundos: Int): Medidas = Medidas(0, 0.0, segundos, kilometros)
}

sealed trait Serie {
    def repeticiones: Int = this match
        case SerieConPeso(r, _) => r
        case SerieDeRepeticiones(r) => r
        case _ => 0

    def peso: Double = this match
        case SerieConPeso(_, p) => p
        case _ => 0.0

    def segundos: Int = this match
        case SerieDeTiempo(s) => s
        case SerieDeDistanciaYTiempo(_, s) => s
        case _ => 0

    def kilometros: Double = this match
        case SerieDeDistancia(k) => k
        case SerieDeDistanciaYTiempo(k, _) => k
        case _ => 0.0

    def volumen: Double = repeticiones * peso

    def marca: Double = this match
        case SerieConPeso(_, p) => p
        case SerieDeRepeticiones(r) => r.toDouble
        case SerieDeTiempo(s) => s.toDouble
        case SerieDeDistancia(k) => k
        case SerieDeDistanciaYTiempo(k, _) => k
}

case class SerieConPeso private[modelo](override val repeticiones: Int, override val peso: Double) extends Serie
case class SerieDeRepeticiones private[modelo](override val repeticiones: Int) extends Serie
case class SerieDeTiempo private[modelo](override val segundos: Int) extends Serie
case class SerieDeDistancia private[modelo](override val kilometros: Double) extends Serie
case class SerieDeDistanciaYTiempo private[modelo](override val kilometros: Double, override val segundos: Int) extends Serie

object Serie {
    private def pesoValido(peso: Double): Boolean = java.lang.Double.isFinite(peso) && peso > 0.0
    private def distanciaValida(km: Double): Boolean = java.lang.Double.isFinite(km) && km > 0.0

    def crear(medicion: Medicion, datos: Medidas): Resultado[Serie] = medicion match
        case RepeticionesConPeso if datos.repeticiones > 0 && pesoValido(datos.peso) =>
            Exito(SerieConPeso(datos.repeticiones, datos.peso))
        case SoloRepeticiones if datos.repeticiones > 0 =>
            Exito(SerieDeRepeticiones(datos.repeticiones))
        case PorTiempo if datos.segundos > 0 =>
            Exito(SerieDeTiempo(datos.segundos))
        case PorDistancia if distanciaValida(datos.kilometros) =>
            Exito(SerieDeDistancia(datos.kilometros))
        case DistanciaYTiempo if distanciaValida(datos.kilometros) && datos.segundos > 0 =>
            Exito(SerieDeDistanciaYTiempo(datos.kilometros, datos.segundos))
        case RepeticionesConPeso => Fallo("Ingresa repeticiones y un peso mayor que cero.")
        case SoloRepeticiones => Fallo("Las repeticiones deben ser al menos 1.")
        case PorTiempo => Fallo("El tiempo debe ser mayor que cero.")
        case PorDistancia => Fallo("La distancia debe ser mayor que cero.")
        case DistanciaYTiempo => Fallo("Ingresa una distancia y un tiempo mayores que cero.")
}
