# ----------------------------
# Makefile Options
# ----------------------------

NAME = CMAT
ICON = icon.png
DESCRIPTION = "Complex matrix calculator"
COMPRESSED = NO

CFLAGS = -Wall -Wextra -Oz
CXXFLAGS = -Wall -Wextra -Oz

# ----------------------------

include $(shell cedev-config --makefile)

