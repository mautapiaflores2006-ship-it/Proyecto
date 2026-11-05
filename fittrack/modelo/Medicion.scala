package fittrack.modelo

sealed trait Medicion

case object RepeticionesConPeso extends Medicion
case object SoloRepeticiones extends Medicion
case object PorTiempo extends Medicion
case object PorDistancia extends Medicion
case object DistanciaYTiempo extends Medicion

object Medicion {

    val todas: List[Medicion] =
        List(RepeticionesConPeso, SoloRepeticiones, PorTiempo, PorDistancia, DistanciaYTiempo)

    def nombre(medicion: Medicion): String = {
        medicion match
            case RepeticionesConPeso => "Repeticiones y peso"
            case SoloRepeticiones => "Solo repeticiones"
            case PorTiempo => "Tiempo"
            case PorDistancia => "Distancia"
            case DistanciaYTiempo => "Distancia y tiempo"
    }
}
