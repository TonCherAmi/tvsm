# Copyright © 2017 Vasili Karaev
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

CONFIG         := config
CONFIG_DESTDIR := ${DESTDIR}${PREFIX}/share/doc/${PROGNAME}

GUILEINC := $(shell guile -c "(display (%site-dir))")
GUILELIB := $(shell guile -c "(display (%site-ccache-dir))")

GUILEC := guild compile

SRC := $(wildcard ${PROGNAME}/*.scm)
OBJ := ${SRC:.scm=.go}

export GUILE_LOAD_PATH = ${CURDIR}

OBJ: ${SRC}
	$(foreach object, ${OBJ}, ${GUILEC} -o ${object} ${object:.go=.scm};)

all:
	${OBJ}

install:
	mkdir -p ${GUILEINC}/${PROGNAME}
	echo installing source files in ${GUILEINC}/${PROGNAME}
	cp -p ${SRC} ${GUILEINC}/${PROGNAME}
	
	mkdir -p ${GUILELIB}/${PROGNAME}
	echo installing object files in ${GUILELIB}/${PROGNAME}
	cp -p ${OBJ} ${GUILELIB}/${PROGNAME}
	
	mkdir -p ${DESTDIR}${PREFIX}/bin
	echo installing executable script in ${DESTDIR}${PREFIX}/bin
	cp -p scripts/${PROGNAME} ${DESTDIR}${PREFIX}/bin

	mkdir -p ${CONFIG_DESTDIR}
	cp -p ${CONFIG} ${CONFIG_DESTDIR}/${CONFIG}

uninstall:
	rm -rf ${GUILEINC}/${PROGNAME} 
	rm -rf ${GUILELIB}/${PROGNAME} 
	rm -f  ${DESTDIR}${PREFIX}/bin/${PROGNAME}
	rm -rf ${CONFIG_DESTDIR}

clean:
	rm ${OBJ}
