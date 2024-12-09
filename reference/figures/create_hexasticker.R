library(hexSticker)
sticker("man/figures/logo-raw.png", package = "gcube",
        p_size = 27, p_color = "#48A529",
        p_x = 1, p_y = 1.5,
        s_x = 1, s_y = 0.7, s_width = 0.5, s_height = 0.5,
        h_fill = "black", h_color = "#48A529",
        url = "https://b-cubed-eu.github.io/gcube/",
        filename = "man/figures/hexa-sticker.png")

usethis::use_logo("man/figures/hexa-sticker.png")
