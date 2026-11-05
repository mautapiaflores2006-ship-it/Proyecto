package fittrack.modelo

case class Rutina(
    nombre: String,
    ejercicios: List[Ejercicio]
) {
    def agregarEjercicio(ejercicio: Ejercicio): Rutina =
        if contiene(ejercicio) then this
        else copy(ejercicios = ejercicios :+ ejercicio)

    def quitarEjercicio(ejercicio: Ejercicio): Rutina =
        copy(ejercicios = ejercicios.filter(e => e != ejercicio))

    def contiene(ejercicio: Ejercicio): Boolean =
        ejercicios.filter(e => e == ejercicio).nonEmpty

    def cantidadEjercicios: Int = ejercicios.length
}
