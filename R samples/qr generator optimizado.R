library(qrcode)

# URL to encode
encuesta_estudiantes <- "https://ee.kobotoolbox.org/x/t5F7gW5O"
encuesta_docentes <- "https://ee.kobotoolbox.org/x/ec9WHXqh"

# Generate QR code
qr_estudiantes <- qr_code(encuesta_estudiantes)
qr_docentes <- qr_code(encuesta_docentes)

# Display QR code

# Save QR estudiantes with title
png("qr_estudiantes.png", width = 800, height = 850)
par(mar = c(1, 1, 6, 1)) # extra space on top for title
plot(qr_estudiantes, axes = FALSE, xlab = "", ylab = "")
title("Encuesta para estudiantes", line = 2, cex.main = 4)
dev.off()

# Save QR docentes with title
png("qr_docentes.png", width = 800, height = 850)
par(mar = c(1, 1, 6, 1)) # extra space on top for title
plot(qr_docentes, axes = FALSE, xlab = "", ylab = "")
title("Encuesta para docentes y directores", line = 2, cex.main = 3)
dev.off()
