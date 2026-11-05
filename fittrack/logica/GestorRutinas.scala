package fittrack.logica

import fittrack.modelo.*
import fittrack.util.*

object GestorRutinas {

    def filtrarEjercicios(
        catalogo: List[Ejercicio],
        criterio: Ejercicio => Boolean
    ): List[Ejercicio] =
        catalogo.filter(criterio)

    def filtrarCatalogo(
        catalogo: List[Ejercicio],
        texto: String,
        grupo: Option[GrupoMuscular],
        tipo: Option[TipoEjercicio]
    ): List[Ejercicio] =
        filtrarEjercicios(catalogo, ejercicio =>
            ejercicio.coincideCon(texto) &&
            (grupo match
                case Some(valor) => ejercicio.grupo == valor
                case None => true
            ) &&
            (tipo match
                case Some(valor) => ejercicio.tipo == valor
                case None => true
            )
        )

    def existeEjercicio(catalogo: List[Ejercicio], nombre: String): Boolean =
        catalogo.filter(_.nombre.equalsIgnoreCase(nombre)).nonEmpty

    def registrarEjercicio(catalogo: List[Ejercicio], ejercicio: Ejercicio): List[Ejercicio] =
        catalogo :+ ejercicio

    def eliminarEjercicio(catalogo: List[Ejercicio], ejercicio: Ejercicio): List[Ejercicio] =
        catalogo.filter(actual => actual != ejercicio)

    def actualizarRutina(
        rutinas: List[Rutina],
        nombre: String
    )(cambio: Rutina => Rutina): List[Rutina] =
        rutinas.map(rutina =>
            if rutina.nombre == nombre then cambio(rutina)
            else rutina
        )

    def crearRutina(nombre: String): Resultado[Rutina] =
        if nombre.trim.isEmpty then Fallo("Escribe un nombre para la rutina.")
        else Exito(Rutina(nombre.trim, Nil))

    def existeRutina(rutinas: List[Rutina], nombre: String): Boolean =
        rutinas.filter(_.nombre.equalsIgnoreCase(nombre)).nonEmpty

    def agregarEjercicioARutina(rutina: Rutina, ejercicio: Ejercicio): Rutina =
        rutina.agregarEjercicio(ejercicio)

    // Añade una rutina y reemplaza otra con el mismo nombre.
    def registrarRutina(rutinas: List[Rutina], rutina: Rutina): List[Rutina] =
        rutinas.filter(actual => !actual.nombre.equalsIgnoreCase(rutina.nombre)) :+ rutina

    def eliminarRutina(rutinas: List[Rutina], nombre: String): List[Rutina] =
        rutinas.filter(_.nombre != nombre)

    def crearSesion(fecha: Fecha, rutina: Option[Rutina]): Sesion = {
        val registros = rutina match
            case Some(valor) => valor.ejercicios.map(ejercicio => RegistroEjercicio(ejercicio, Nil))
            case None => Nil

        Sesion(fecha, registros, rutina.map(_.nombre))
    }

    def registrarSesion(historial: List[Sesion], sesion: Sesion): List[Sesion] =
        historial :+ sesion

    def agregarSerieASesion(
        sesion: Sesion,
        ejercicio: Ejercicio,
        serie: Serie
    ): Sesion =
        if sesion.incluye(ejercicio) then
            sesion.copy(registros = sesion.registros.map(registro =>
                if registro.ejercicio == ejercicio then registro.agregarSerie(serie)
                else registro
            ))
        else
            sesion.agregarRegistro(RegistroEjercicio(ejercicio, List(serie)))

    def quitarUltimaSerieDeSesion(sesion: Sesion, ejercicio: Ejercicio): Sesion =
        sesion.copy(registros = sesion.registros.map(registro =>
            if registro.ejercicio == ejercicio then registro.quitarUltimaSerie
            else registro
        ))

    def limpiarSesion(sesion: Sesion): Sesion =
        sesion.copy(registros = sesion.registros.filter(registro => !registro.sinSeries))

    def eliminarSesion(historial: List[Sesion], sesion: Sesion): List[Sesion] =
        historial.filter(actual => actual != sesion)
}
