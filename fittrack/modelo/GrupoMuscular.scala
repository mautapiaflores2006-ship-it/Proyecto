package fittrack.modelo

sealed trait GrupoMuscular

case object Pecho extends GrupoMuscular
case object Espalda extends GrupoMuscular
case object Hombros extends GrupoMuscular
case object Brazos extends GrupoMuscular
case object Piernas extends GrupoMuscular
case object Core extends GrupoMuscular
case object CuerpoCompleto extends GrupoMuscular

object GrupoMuscular {

    val todos: List[GrupoMuscular] =
        List(Pecho, Espalda, Hombros, Brazos, Piernas, Core, CuerpoCompleto)

    def nombre(grupo: GrupoMuscular): String = {
        grupo match
            case Pecho => "Pecho"
            case Espalda => "Espalda"
            case Hombros => "Hombros"
            case Brazos => "Brazos"
            case Piernas => "Piernas"
            case Core => "Abdomen y core"
            case CuerpoCompleto => "Cuerpo completo"
    }
}
