package fittrack.modelo

sealed trait TipoEjercicio

case object Fuerza extends TipoEjercicio
case object Resistencia extends TipoEjercicio
case object Movilidad extends TipoEjercicio

object TipoEjercicio {

    val todos: List[TipoEjercicio] = List(Fuerza, Resistencia, Movilidad)

    def nombre(tipo: TipoEjercicio): String = {
        tipo match
            case Fuerza => "Fuerza"
            case Resistencia => "Resistencia"
            case Movilidad => "Movilidad"
    }
}
