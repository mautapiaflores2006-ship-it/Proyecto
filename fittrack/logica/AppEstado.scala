package fittrack.logica

import fittrack.modelo.*
import fittrack.util.*

case class AppEstado(
    catalogo: List[Ejercicio],
    rutinas: List[Rutina],
    historial: List[Sesion],
    sesionEnCurso: Option[Sesion]
) {
    def agregarEjercicio(
        nombre: String,
        grupo: GrupoMuscular,
        tipo: TipoEjercicio,
        medicion: Medicion
    ): Resultado[AppEstado] =
        Ejercicio.crear(nombre, grupo, tipo, medicion) match
            case Fallo(mensaje) => Fallo(mensaje)
            case Exito(ejercicio) =>
                if GestorRutinas.existeEjercicio(catalogo, ejercicio.nombre) then
                    Fallo("Ya tienes un ejercicio con ese nombre.")
                else
                    Exito(copy(catalogo = GestorRutinas.registrarEjercicio(catalogo, ejercicio)))

    def eliminarEjercicio(ejercicio: Ejercicio): AppEstado =
        copy(
            catalogo = GestorRutinas.eliminarEjercicio(catalogo, ejercicio),
            rutinas = rutinas.map(_.quitarEjercicio(ejercicio))
        )

    def crearRutina(nombre: String): Resultado[AppEstado] =
        GestorRutinas.crearRutina(nombre) match
            case Fallo(mensaje) => Fallo(mensaje)
            case Exito(rutina) =>
                if GestorRutinas.existeRutina(rutinas, rutina.nombre) then
                    Fallo("Ya tienes una rutina con ese nombre.")
                else
                    Exito(copy(rutinas = GestorRutinas.registrarRutina(rutinas, rutina)))

    def eliminarRutina(nombre: String): AppEstado =
        copy(rutinas = GestorRutinas.eliminarRutina(rutinas, nombre))

    def agregarEjercicioARutina(nombre: String, ejercicio: Ejercicio): AppEstado =
        copy(rutinas = GestorRutinas.actualizarRutina(rutinas, nombre)(_.agregarEjercicio(ejercicio)))

    def quitarEjercicioDeRutina(nombre: String, ejercicio: Ejercicio): AppEstado =
        copy(rutinas = GestorRutinas.actualizarRutina(rutinas, nombre)(_.quitarEjercicio(ejercicio)))

    def iniciarSesion(
        anio: Int,
        mes: Int,
        dia: Int,
        rutina: Option[Rutina]
    ): Resultado[AppEstado] = {
        val rutinaVacia = rutina match
            case Some(valor) => valor.ejercicios.isEmpty
            case None => false

        if sesionEnCurso.isDefined then
            Fallo("Ya tienes una sesión en curso.")
        else if rutinaVacia then
            Fallo("Esa rutina todavía no tiene ejercicios.")
        else
            Fecha.crear(anio, mes, dia).map(fecha =>
                copy(sesionEnCurso = Some(GestorRutinas.crearSesion(fecha, rutina)))
            )
    }

    def agregarSerie(ejercicio: Ejercicio, medidas: Medidas): Resultado[AppEstado] =
        sesionEnCurso match
            case None => Fallo("No hay una sesión en curso.")
            case Some(sesion) if sesion.nombreRutina.isDefined && !sesion.incluye(ejercicio) =>
                Fallo("Ese ejercicio no forma parte de la rutina de esta sesión.")
            case Some(sesion) =>
                Serie.crear(ejercicio.medicion, medidas).map(serie =>
                    copy(sesionEnCurso = Some(
                        GestorRutinas.agregarSerieASesion(sesion, ejercicio, serie)
                    ))
                )

    def quitarUltimaSerie(ejercicio: Ejercicio): AppEstado =
        sesionEnCurso match
            case None => this
            case Some(sesion) =>
                copy(sesionEnCurso = Some(
                    GestorRutinas.quitarUltimaSerieDeSesion(sesion, ejercicio)
                ))

    def guardarSesion: Resultado[AppEstado] =
        sesionEnCurso match
            case None => Fallo("No hay una sesión en curso.")
            case Some(sesion) =>
                val sesionLimpia = GestorRutinas.limpiarSesion(sesion)
                if sesionLimpia.estaVacia then
                    Fallo("Añade al menos una serie antes de guardar.")
                else
                    Exito(copy(
                        historial = GestorRutinas.registrarSesion(historial, sesionLimpia),
                        sesionEnCurso = None
                    ))

    def descartarSesion: AppEstado = copy(sesionEnCurso = None)

    def eliminarSesion(sesion: Sesion): AppEstado =
        copy(historial = GestorRutinas.eliminarSesion(historial, sesion))

    def conHistorialDeEjemplo: AppEstado =
        copy(historial = DatosIniciales.historialDemo)
}

object AppEstado {
    val inicial: AppEstado =
        AppEstado(DatosIniciales.catalogoInicial, DatosIniciales.rutinasIniciales, Nil, None)
}
