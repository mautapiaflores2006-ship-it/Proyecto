package fittrack.modelo

case class Sesion(
    fecha: Fecha,
    registros: List[RegistroEjercicio],
    nombreRutina: Option[String]
) {
    def agregarRegistro(registro: RegistroEjercicio): Sesion =
        copy(registros = registros :+ registro)

    def volumenTotal: Double =
        registros.foldLeft(0.0)((total, registro) => total + registro.volumen)

    def repeticionesTotales: Int =
        registros.foldLeft(0)((total, registro) => total + registro.repeticiones)

    def tiempoTotal: Int =
        registros.foldLeft(0)((total, registro) => total + registro.segundos)

    def distanciaTotal: Double =
        registros.foldLeft(0.0)((total, registro) => total + registro.kilometros)

    def totalSeries: Int =
        registros.foldLeft(0)((total, registro) => total + registro.series.length)

    def estaVacia: Boolean = totalSeries == 0

    def incluye(ejercicio: Ejercicio): Boolean =
        registros.filter(registro => registro.ejercicio == ejercicio).nonEmpty
}
