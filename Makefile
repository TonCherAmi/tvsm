# Copyright © 2017-2018 Vasili Karaev
#
# This file is part of tvsm.
#
# tvsm is free software: you can redistribute  it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# tvsm is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of 
# MERCHENTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with tvsm. If not, see <http://www.gnu.org/licenses/>.

PROGNAME := tvsm

PREFIX := /usr

SCRIPTDIR := scripts

CONFIG         := config
CONFIG_DESTDIR := ${DESTDIR}${PREFIX}/share/doc/${PROGNAME}

GUILEINC := ${DESTDIR}$(shell guile -c "(display (%site-dir))")
GUILELIB := ${DESTDIR}$(shell guile -c "(display (%site-ccache-dir))")

GUILEC := guild compile

SUBDIRS := $(shell find ${PROGNAME} -type d -print)
SRC := $(foreach subdir, ${SUBDIRS}, $(wildcard ${subdir}/*.scm))
OBJ := ${SRC:.scm=.go}

.SILENT: OBJ

export GUILE_AUTO_COMPILE = 0
export GUILE_LOAD_PATH = ${CURDIR}

OBJ: ${SRC}
	$(foreach object, ${OBJ}, ${GUILEC} -o ${object} ${object:.go=.scm} 2>/dev/null;)

all:
	${OBJ}

install:
	@mkdir -p ${GUILEINC}
	@echo installing source files in ${GUILEINC}/${PROGNAME}
	@cp -p --parents ${SRC} ${GUILEINC}
	
	@mkdir -p ${GUILELIB}
	@echo installing object files in ${GUILELIB}/${PROGNAME}
	@cp -p --parents ${OBJ} ${GUILELIB}
	
	@mkdir -p ${DESTDIR}${PREFIX}/bin
	@echo installing executable script in ${DESTDIR}${PREFIX}/bin
	@cp -p ${SCRIPTDIR}/${PROGNAME} ${DESTDIR}${PREFIX}/bin/${PROGNAME}

	@mkdir -p ${CONFIG_DESTDIR}
	@echo installing default config in ${CONFIG_DESTDIR}
	@cp -p ${CONFIG} ${CONFIG_DESTDIR}/${CONFIG}

uninstall:
	@rm -rf ${GUILEINC}/${PROGNAME}
	@rm -rf ${GUILELIB}/${PROGNAME}
	@rm -f  ${DESTDIR}${PREFIX}/bin/${PROGNAME}
	@rm -rf ${CONFIG_DESTDIR}

clean:
	@rm ${OBJ}
