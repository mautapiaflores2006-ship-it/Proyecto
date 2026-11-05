package fittrack.modelo

case class RegistroEjercicio(ejercicio: Ejercicio, series: List[Serie]) {
    def agregarSerie(serie: Serie): RegistroEjercicio = copy(series = series :+ serie)
    def quitarUltimaSerie: RegistroEjercicio = copy(series = series.dropRight(1))

    def volumen: Double = series.foldLeft(0.0)((total, serie) => total + serie.volumen)
    def repeticiones: Int = series.foldLeft(0)((total, serie) => total + serie.repeticiones)
    def segundos: Int = series.foldLeft(0)((total, serie) => total + serie.segundos)
    def kilometros: Double = series.foldLeft(0.0)((total, serie) => total + serie.kilometros)
    def mejorPeso: Double = series.foldLeft(0.0)((mayor, serie) => math.max(mayor, serie.peso))
    def mejorMarca: Double = series.foldLeft(0.0)((mayor, serie) => math.max(mayor, serie.marca))
    def sinSeries: Boolean = series.isEmpty
}
