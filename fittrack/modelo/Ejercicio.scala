package fittrack.modelo

import fittrack.util.*

case class Ejercicio(
    nombre: String,
    grupo: GrupoMuscular,
    tipo: TipoEjercicio,
    medicion: Medicion
) {
    def coincideCon(texto: String): Boolean =
        nombre.toLowerCase.contains(texto.toLowerCase) ||
        GrupoMuscular.nombre(grupo).toLowerCase.contains(texto.toLowerCase)

}

object Ejercicio {
    def crear(nombre: String, grupo: GrupoMuscular, tipo: TipoEjercicio, medicion: Medicion): Resultado[Ejercicio] =
        if nombre.trim.isEmpty then Fallo("Escribe un nombre para el ejercicio.")
        else Exito(Ejercicio(nombre.trim, grupo, tipo, medicion))
}
