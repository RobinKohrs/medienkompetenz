# ==============================================================================
# Skript zur kombinierten Visualisierung von arithmetischem Mittel und Median
# auf einer einzigen horizontalen Linie.
# ==============================================================================

# --- 1. Vorbereitung: Pakete installieren und laden ---

# Prüfen, ob ggplot2 installiert ist. Wenn nicht, wird es installiert.
if (!require("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

if (!require("paletteer", quietly = TRUE)) {
  install.packages("paletteer")
}

if (!require("here", quietly = TRUE)) {
  install.packages("here")
}

library(paletteer)
library(here)

# Laden der Bibliotheken
library(ggplot2)

# --- 2. Datenerstellung ---

# Erstellen eines Datenrahmens (data.frame) mit den Beispiel-Taschengeldern.
taschengeld_daten <- data.frame(
  Person = c("Anna", "Ben", "Clara", "David", "Emilia"),
  Taschengeld = c(15, 20, 25, 30, 500)
)

# --- 3. Berechnungen ---

# Berechnung des arithmetischen Mittels (Durchschnitt)
mittelwert <- mean(taschengeld_daten$Taschengeld)

# Berechnung des Medians (der Wert in der Mitte der sortierten Daten)
median_wert <- median(taschengeld_daten$Taschengeld)

# Ausgabe der berechneten Werte in der Konsole zur Kontrolle
print(paste("Das arithmetische Mittel beträgt:", mittelwert, "€"))
print(paste("Der Median beträgt:", median_wert, "€"))

# --- 4. Erstellung der kombinierten Grafik ---

# Erstellen des kombinierten Plots

plot_kombiniert_final <- ggplot(
  taschengeld_daten,
  aes(x = Taschengeld, y = 0)
) +

  # Basislinie
  geom_hline(yintercept = 0, color = "darkgray", linewidth = 0.8) +

  # Vertikale Linien für Mittelwert und Median
  geom_vline(
    aes(xintercept = mittelwert, color = "Arithmetisches Mittel"),
    linetype = "11",
    linewidth = 1
  ) +
  geom_vline(
    aes(xintercept = median_wert, color = "Median"),
    linetype = "11",
    linewidth = 1
  ) +

  # Datenpunkte
  geom_point(
    size = 6,
    aes(fill = Person),
    shape = 21,
    color = "black",
    alpha = 0.8
  ) +
  scale_fill_paletteer_d("ggthemes::Winter") +

  # Manuelle Definition der Farben für die Linien
  scale_color_manual(
    values = c("Arithmetisches Mittel" = "tomato", "Median" = "cornflowerblue")
  ) +

  # Skalierung der X-Achse
  scale_x_continuous(
    breaks = c(0, 25, 50, 118, 150, seq(200, 500, 100)),
    limits = c(0, 550)
  ) +

  # Steuerung der Legenden-Darstellung
  guides(
    color = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      direction = "horizontal"
    ),
    fill = guide_legend(
      title = "Person",
      title.position = "top",
      title.hjust = 0.5,
      direction = "horizontal",
      nrow = 1
    )
  ) +

  # Titel, Untertitel und Beschriftungen
  labs(
    title = "Der trügerische Durchschnitt: Mittelwert vs. Median",
    subtitle = paste(
      "Datenpunkte für:",
      paste(taschengeld_daten$Person, collapse = ", ")
    ),
    x = "Taschengeld in €",
    caption = "Der Median (blau) beschreibt die Realität der meisten Personen besser als das durch den Ausreißer verzerrte Mittel (rot).",
    color = "Kennzahlen" # Setzt den Titel für die Farblegende
  ) +

  # Theme-Anpassungen für das finale Layout
  theme_minimal(base_size = 16) +
  theme(
    # Y-Achse ausblenden
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),

    # Titel- und Caption-Ausrichtung
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0),
    plot.subtitle = element_text(hjust = 0),
    plot.caption = element_text(hjust = 0),
    plot.caption.position = "panel",

    # Legenden-Positionierung
    legend.position = "top",
    legend.justification = "left",
    legend.box = "horizontal",
    legend.box.spacing = unit(0.5, "cm") # Fügt etwas Abstand zwischen Legende und Titel hinzu
  )

# Anzeigen und Speichern der Grafik
print(plot_kombiniert_final)
ggsave(
  "grafik_kombiniert_final_layout.png",
  plot = plot_kombiniert_final,
  width = 10,
  height = 4,
  dpi = 300,
  path = here("graphics")
)

print(
  "Skript erfolgreich ausgeführt. Die finale Grafik mit benutzerdefiniertem Layout wurde gespeichert."
)
