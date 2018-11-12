Public Class MultiTrimmerKnob

    Const Pi As Double = Math.PI
        Dim x1, y1, x2, y2 As Integer
    Dim xp1, yp1, xp2, yp2, xp3, yp3, xp4, yp4 As Int32
    Private m_value As Integer
    Private m_minimum As Integer
    Private m_maximum As Integer
    Dim centreX As Double = 32
    Dim centreY As Double = 32
    Dim apen As New Pen(Color.LightGray, 1)
    Dim bpen As New Pen(Color.Gray, 1)
    Dim cpen As New Pen(Color.Black, 1)
    Dim lpen As New Pen(Color.Black, 2)
    Dim tpen As New Pen(Color.Transparent, 2)
    Dim radius As Double = 30
    Dim blinkingLedTimer As New Timer
    Dim mouseDownLocation As New Point
    Dim eventString As String
    Public Property value As Integer
        Get
            value = m_value
        End Get
        Set(value As Integer)
            If value >= minimum And value <= maximum Then
                m_value = value
            Else
                Exit Property
            End If
        End Set
    End Property
    Public Property minimum As Integer
        Get
            minimum = m_minimum
        End Get
        Set(minimum As Integer)
            If minimum > maximum Then
                MsgBox("Min can't be greater than Max")
                Exit Property
            End If
            m_minimum = minimum
            If value < minimum Then
                value = minimum
            End If
            For Each lbl In Me.Controls
                Select Case lbl.tag
                    Case "lbl0"
                        lbl.text = minimum
                    Case "lbl2"
                        lbl.text = (maximum - minimum) / 10 * (2.ToString.PadLeft(2, "0")) + minimum
                    Case "lbl4"
                        lbl.text = (maximum - minimum) / 10 * (4.ToString.PadLeft(2, "0")) + minimum
                    Case "lbl6"
                        lbl.text = (maximum - minimum) / 10 * (6.ToString.PadLeft(2, "0")) + minimum
                    Case "lbl8"
                        lbl.text = (maximum - minimum) / 10 * (8.ToString.PadLeft(2, "0")) + minimum
                    Case "lbl10"
                        lbl.text = (maximum - minimum) / 10 * (10.ToString.PadLeft(2, "0")) + minimum
                End Select
            Next
            Dim c = value
            value = c
            Me.Refresh()
        End Set
    End Property
    Public Property maximum As Integer
            Get
                maximum = m_maximum
            End Get
            Set(maximum As Integer)
                If maximum < minimum Then
                    MsgBox("Max can't be smaller than Min")
                    Exit Property
                End If
                m_maximum = maximum
                If value > maximum Then
                    value = maximum
                End If
                For Each lbl In Me.Controls
                    Select Case lbl.tag
                        Case "lbl0"
                            lbl.text = minimum
                        Case "lbl2"
                            lbl.text = (maximum - minimum) / 10 * (2.ToString.PadLeft(2, "0")) + minimum
                        Case "lbl4"
                            lbl.text = (maximum - minimum) / 10 * (4.ToString.PadLeft(2, "0")) + minimum
                        Case "lbl6"
                            lbl.text = (maximum - minimum) / 10 * (6.ToString.PadLeft(2, "0")) + minimum
                        Case "lbl8"
                            lbl.text = (maximum - minimum) / 10 * (8.ToString.PadLeft(2, "0")) + minimum
                        Case "lbl10"
                            lbl.text = (maximum - minimum) / 10 * (10.ToString.PadLeft(2, "0")) + minimum
                    End Select
                Next
                Dim c = value
                value = c
                Me.Refresh()
            End Set
        End Property
    Private Sub GaugePaint(sender As Object, e As PaintEventArgs) Handles Me.Paint

        For num As Double = -5 / 4 * Pi To 1 / 4 * Pi Step 0.075
            x1 = Convert.ToInt32(radius * Math.Cos(num) + centreX)
            y1 = Convert.ToInt32(radius * Math.Sin(num) + centreY)
            e.Graphics.DrawLine(apen, x1, y1, x1 + 1, y1)
        Next
        Dim i As Integer = 0
        For num As Double = -5 / 4 * Pi To 1 / 4 * Pi Step 0.15 * Pi
            x1 = Convert.ToInt32(radius * Math.Cos(num) + centreX)
            y1 = Convert.ToInt32(radius * Math.Sin(num) + centreY)
            x2 = Convert.ToInt32(3 / 4 * radius * Math.Cos(num) + centreX)
            y2 = Convert.ToInt32(3 / 4 * radius * Math.Sin(num) + centreY)

            e.Graphics.DrawLine(apen, x1, y1, x2, y2)
            addlabel(i)
            i += 1
        Next

        redrawLine(e)

    End Sub
    Private Sub addlabel(ByVal i As Integer)
            Dim lblScale As New Label
            With lblScale
                .Location = New Point(x2 - 5, y2 - 3)
                .Size = New Size(18, 10)
                .Text = (maximum - minimum) / 10 * (i.ToString.PadLeft(2, "0"))
                .Font = New Font("Segoe UI", 5, FontStyle.Regular)
                .ForeColor = Color.Black
                .BackColor = Color.Transparent
                .TextAlign = ContentAlignment.TopLeft
                .Tag = "lbl" & i.ToString
            End With
            For Each lbl In Me.Controls
                If TypeOf (lbl) Is Label And lbl.tag = "lbl" & i.ToString Then
                    Exit Sub
                End If
            Next
            If i Mod 2 = 0 Then
                Me.Controls.Add(lblScale)
            End If
        End Sub
    Sub New()

        ' La chiamata è richiesta dalla finestra di progettazione.
        InitializeComponent()

        ' Aggiungere le eventuali istruzioni di inizializzazione dopo la chiamata a InitializeComponent().
        minimum = 0
        maximum = 100
        value = 0

    End Sub
    Public Sub redrawLine(e As PaintEventArgs)
            Dim Lend As Double = 1 / 4 * Pi - 6 / 4 * Pi * ((maximum - value) / (maximum - minimum))
            Dim Lstart = Lend - Pi


        xp1 = Convert.ToInt32(14 * Math.Cos(Lend) + centreX)
        yp1 = Convert.ToInt32(14 * Math.Sin(Lend) + centreY)
        xp2 = Convert.ToInt32(8 * Math.Cos(Lend - 0.5) + centreX)
        yp2 = Convert.ToInt32(8 * Math.Sin(Lend - 0.5) + centreY)
        xp3 = Convert.ToInt32(8 * Math.Cos(Lend + 0.5) + centreX)
        yp3 = Convert.ToInt32(8 * Math.Sin(Lend + 0.5) + centreY)

        Dim valorediPartenza As Double = value / maximum * Pi

        For m = valorediPartenza To 2 * Pi + valorediPartenza Step Pi / 8
            xp4 = Convert.ToInt32(14 * Math.Cos(m) + centreX - 1.5)
            yp4 = Convert.ToInt32(14 * Math.Sin(m) + centreY - 1.5)

            e.Graphics.DrawArc(cpen, xp4, yp4, 3, 3, CSng(m * 180 / Pi + 90), 180)
        Next

        e.Graphics.DrawEllipse(cpen, 18, 18, 28, 28)

        Dim points As Point() = {New Point(xp1, yp1), New Point(xp2, yp2), New Point(xp3, yp3)}

        e.Graphics.FillPolygon(Brushes.Black, points)

    End Sub
    Private Sub Me_MouseDown(sender As Object, e As System.Windows.Forms.MouseEventArgs) Handles Me.MouseDown

        eventString = ""
        Select Case e.Button
            Case MouseButtons.Left
                eventString = "L"
                ' Update the mouse path with the mouse information
                mouseDownLocation = New Point(e.X, e.Y)
            Case Else
                eventString = ""
                mouseDownLocation = Nothing
        End Select

    End Sub
    Private Sub Me_MouseUp(sender As Object, e As System.Windows.Forms.MouseEventArgs) Handles Me.MouseUp

        eventString = ""

    End Sub
    Private Sub Me_MouseMove(sender As Object, e As System.Windows.Forms.MouseEventArgs) Handles Me.MouseMove

        If eventString = "L" Then
            Dim mouseX As Integer = e.X
            Dim mouseY As Integer = e.Y
            If mouseX > mouseDownLocation.X And mouseY < centreY Then
                value += maximum / 20
                mouseDownLocation = New Point(e.X, e.Y)
            ElseIf mouseX < mouseDownLocation.X And mouseY > centreY Then
                value += maximum / 20
                mouseDownLocation = New Point(e.X, e.Y)
            ElseIf mouseX > mouseDownLocation.X And mouseY > centreY Then
                value -= maximum / 20
                mouseDownLocation = New Point(e.X, e.Y)
            ElseIf mouseX < mouseDownLocation.X And mouseY < centreY Then
                value -= maximum / 20
                mouseDownLocation = New Point(e.X, e.Y)
            End If
        Else
            Exit Sub
        End If
        Me.Refresh()
    End Sub
End Class
