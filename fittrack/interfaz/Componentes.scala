package fittrack.interfaz

import java.awt.{Color, Component, Cursor, Dimension, GridBagConstraints, GridBagLayout, Graphics, Insets, LayoutManager}
import javax.swing.*
import javax.swing.event.{DocumentEvent, DocumentListener}

/** Un valor de tipo A junto al texto con el que se muestra en listas y selectores. */
class Opcion[A](val valor: A, texto: String) {
    override def toString: String = texto
}

sealed trait EstiloBoton
case object Primario extends EstiloBoton
case object Secundario extends EstiloBoton
case object Peligro extends EstiloBoton

/** Botón plano: un rectángulo de color sólido, sin degradado ni sombra. */
class BotonPlano(texto: String, estilo: EstiloBoton) extends JButton(texto) {
    setFont(Tema.fuenteNegrita)
    setFocusPainted(false)
    setContentAreaFilled(false)
    setBorderPainted(false)
    setOpaque(false)
    setRolloverEnabled(true)
    setForeground(colorTexto)
    setBorder(Tema.margen(12, 22, 12, 22))
    setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR))

    private def colorTexto: Color = {
        estilo match
            case Primario => Tema.fondo
            case Secundario => Tema.tinta
            case Peligro => Tema.alerta
    }

    private def colorRelleno(encima: Boolean): Color = {
        estilo match
            case Primario => if encima then Tema.acentoOscuro else Tema.acento
            case Secundario => if encima then Tema.acentoSuave else Tema.fondo
            case Peligro => if encima then Tema.alertaSuave else Tema.fondo
    }

    override protected def paintComponent(g: Graphics): Unit = {
        g.setColor(colorRelleno(getModel().isRollover()))
        g.fillRect(0, 0, getWidth(), getHeight())
        if estilo != Primario then {
            g.setColor(if estilo == Peligro then Tema.alerta else Tema.linea)
            g.drawRect(0, 0, getWidth() - 1, getHeight() - 1)
        }
        super.paintComponent(g)
    }
}

/** Opción del menú lateral: solo texto, con una barra clara a la izquierda cuando está activa. */
class BotonNavegacion(texto: String) extends JToggleButton(texto) {
    setFont(new java.awt.Font(Tema.familiaCuerpo, java.awt.Font.BOLD, 17))
    setHorizontalAlignment(SwingConstants.LEFT)
    setFocusPainted(false)
    setContentAreaFilled(false)
    setBorderPainted(false)
    setOpaque(false)
    setRolloverEnabled(true)
    setForeground(Tema.lateralTexto)
    setBorder(Tema.margen(15, 30, 15, 20))
    setCursor(Cursor.getPredefinedCursor(Cursor.HAND_CURSOR))

    override protected def paintComponent(g: Graphics): Unit = {
        if isSelected() then {
            g.setColor(Tema.acento)
            g.fillRect(0, 0, getWidth(), getHeight())
            g.setColor(Tema.fondo)
            g.fillRect(0, 0, 6, getHeight())
        } else if getModel().isRollover() then {
            g.setColor(new Color(0x262927))
            g.fillRect(0, 0, getWidth(), getHeight())
        }
        super.paintComponent(g)
    }
}

/** Línea de aviso: verde si salió bien, terracota si hay algo que corregir. */
class Aviso extends JLabel(" ") {
    setFont(Tema.fuenteCuerpo)

    def mostrar(texto: String, esError: Boolean): Unit = {
        setForeground(if esError then Tema.alerta else Tema.acento)
        setText(texto)
    }

    def limpiar(): Unit = setText(" ")
}

/** Lista de valores de tipo A (genérica), con selección por valor y sin tocar índices desde afuera. */
class ListaOpciones[A](autoSeleccionar: Boolean) {

    private val modelo = new DefaultListModel[Opcion[A]]()
    val lista = new JList[Opcion[A]](modelo)

    lista.setFont(Tema.fuenteCuerpo)
    lista.setBackground(Tema.superficie)
    lista.setForeground(Tema.tinta)
    lista.setSelectionBackground(Tema.acentoSuave)
    lista.setSelectionForeground(Tema.tinta)
    lista.setSelectionMode(ListSelectionModel.SINGLE_SELECTION)
    lista.setFixedCellHeight(40)
    lista.setCellRenderer(new DefaultListCellRenderer {
        override def getListCellRendererComponent(l: JList[?], valor: Object, indice: Int, seleccionado: Boolean, foco: Boolean): Component = {
            val c = super.getListCellRendererComponent(l, valor, indice, seleccionado, foco)
            setBorder(Tema.margen(0, 14, 0, 14))
            c
        }
    })

    /** Reemplaza el contenido manteniendo, si se puede, la posición seleccionada. */
    def mostrar(elementos: List[Opcion[A]]): Unit = {
        val anterior = lista.getSelectedIndex()
        modelo.clear()
        elementos.foreach(o => modelo.addElement(o))
        if modelo.size() > 0 then {
            if anterior >= 0 && anterior < modelo.size() then lista.setSelectedIndex(anterior)
            else if autoSeleccionar then lista.setSelectedIndex(0)
        }
    }

    def seleccionado: Option[A] = Option(lista.getSelectedValue()).map(o => o.valor)

    def seleccionarUltimo(): Unit = {
        if modelo.size() > 0 then lista.setSelectedIndex(modelo.size() - 1)
    }

    def alSeleccionar(accion: () => Unit): Unit =
        lista.addListSelectionListener(e => if !e.getValueIsAdjusting() then accion())
}

/** Fábrica de componentes con el estilo de la app. */
object Componentes {

    def lienzo(layout: LayoutManager): JPanel = {
        val p = new JPanel(layout)
        p.setBackground(Tema.fondo)
        p
    }

    /** Panel que apila sus componentes en vertical. */
    def columna(): JPanel = {
        val p = new JPanel()
        p.setLayout(new BoxLayout(p, BoxLayout.Y_AXIS))
        p.setBackground(Tema.fondo)
        p
    }

    def izquierda[T <: JComponent](componente: T): T = {
        componente.setAlignmentX(Component.LEFT_ALIGNMENT)
        componente
    }

    def etiqueta(texto: String, fuente: java.awt.Font, color: Color): JLabel = {
        val l = new JLabel(texto)
        l.setFont(fuente)
        l.setForeground(color)
        l
    }

    def seccion(texto: String): JLabel = {
        val l = etiqueta(texto, Tema.fuenteTitulo, Tema.tinta)
        l.setBorder(Tema.margen(0, 0, 10, 0))
        l
    }

    def campoTexto(columnas: Int): JTextField = {
        val c = new JTextField(columnas)
        c.setFont(Tema.fuenteCuerpo)
        c.setBackground(Tema.superficie)
        c.setForeground(Tema.tinta)
        c.setCaretColor(Tema.tinta)
        c.setBorder(Tema.bordeCampo)
        c
    }

    /** Texto de solo lectura sin caja, para columnas laterales. */
    def parrafo(): JTextArea = {
        val a = new JTextArea()
        a.setEditable(false)
        a.setLineWrap(true)
        a.setWrapStyleWord(true)
        a.setFont(Tema.fuenteCuerpo)
        a.setForeground(Tema.tinta)
        a.setBackground(Tema.fondo)
        a.setBorder(Tema.margen(0, 0, 0, 0))
        a
    }

    /** Texto de solo lectura dentro de una caja con scroll. */
    def areaTexto(): JScrollPane = {
        val a = new JTextArea()
        a.setEditable(false)
        a.setLineWrap(true)
        a.setWrapStyleWord(true)
        a.setFont(Tema.fuenteCuerpo)
        a.setForeground(Tema.tinta)
        a.setBackground(Tema.superficie)
        a.setBorder(Tema.margen(14, 16, 14, 16))
        desplazable(a)
    }

    def textoDe(caja: JScrollPane): JTextArea = caja.getViewport().getView().asInstanceOf[JTextArea]

    def desplazable(contenido: Component): JScrollPane = {
        val s = new JScrollPane(contenido)
        s.setBorder(BorderFactory.createLineBorder(Tema.linea))
        s.getViewport().setBackground(Tema.superficie)
        s
    }

    // ===== Selectores (JComboBox genérico) =====

    def selector[A](opciones: List[Opcion[A]]): JComboBox[Opcion[A]] = {
        val combo = new JComboBox[Opcion[A]]()
        combo.setFont(Tema.fuenteCuerpo)
        combo.setBackground(Tema.superficie)
        combo.setForeground(Tema.tinta)
        opciones.foreach(o => combo.addItem(o))
        combo
    }

    def valorSeleccionado[A](combo: JComboBox[Opcion[A]]): Option[A] = {
        val indice = combo.getSelectedIndex()
        if indice < 0 then None else Some(combo.getItemAt(indice).valor)
    }

    /** Para selectores cuyo valor es "cualquiera" (None) o uno concreto (Some). */
    def valorOpcional[A](combo: JComboBox[Opcion[Option[A]]]): Option[A] = {
        valorSeleccionado(combo) match
            case Some(valor) => valor
            case None => None
    }

    /** Vuelve a llenar el selector conservando, si sigue existiendo, el valor elegido. */
    def llenarSelector[A](combo: JComboBox[Opcion[A]], opciones: List[Opcion[A]]): Unit = {
        val anterior = valorSeleccionado(combo)
        combo.removeAllItems()
        opciones.foreach(o => combo.addItem(o))
        anterior match
            case None => ()
            case Some(valor) =>
                opciones.filter(o => o.valor == valor) match
                    case encontrada :: _ => combo.setSelectedItem(encontrada)
                    case Nil => ()
    }

    /** Formulario en una columna: etiqueta pequeña encima de cada campo. */
    def formulario(campos: List[(String, JComponent)]): JPanel = {
        val p = lienzo(new GridBagLayout())
        val c = new GridBagConstraints()
        c.gridx = 0
        c.weightx = 1.0
        c.fill = GridBagConstraints.HORIZONTAL
        c.anchor = GridBagConstraints.NORTHWEST

        def agregar(restantes: List[(String, JComponent)], fila: Int): Unit = {
            restantes match
                case Nil => ()
                case (texto, componente) :: cola =>
                    c.gridy = fila
                    c.insets = new Insets(0, 0, 4, 0)
                    p.add(etiqueta(texto, Tema.fuentePequena, Tema.tintaSuave), c)
                    c.gridy = fila + 1
                    c.insets = new Insets(0, 0, 16, 0)
                    p.add(componente, c)
                    agregar(cola, fila + 2)
        }

        agregar(campos, 0)
        p
    }

    def alCambiarTexto(campo: JTextField, accion: () => Unit): Unit = {
        campo.getDocument().addDocumentListener(new DocumentListener {
            override def insertUpdate(e: DocumentEvent): Unit = accion()
            override def removeUpdate(e: DocumentEvent): Unit = accion()
            override def changedUpdate(e: DocumentEvent): Unit = accion()
        })
    }

    def confirmar(padre: Component, pregunta: String): Boolean =
        JOptionPane.showConfirmDialog(padre, pregunta, "Confirmar", JOptionPane.YES_NO_OPTION) == JOptionPane.YES_OPTION

    /** (año, mes, día) de hoy: lo único impuro, y solo en el borde de la interfaz. */
    def hoy(): (Int, Int, Int) = {
        val d = java.time.LocalDate.now()
        (d.getYear(), d.getMonthValue(), d.getDayOfMonth())
    }
}
