// Copyright 2017, rightfold
//
// This file is part of CRW.
//
// CRW is free software: you can redistribute it and/or modify it under the
// terms of the GNU Affero General Public License as published by the Free
// Software Foundation, either version 3 of the License, or (at your option) any
// later version.
//
// CRW is distributed in the hope that it will be useful, but WITHOUT ANY
// WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
// A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
// details.
//
// You should have received a copy of the GNU Affero General Public License
// along with CRW. If not, see <http://www.gnu.org/licenses/>.

package Web

import (
	"CRW/RT/Program"
)

type Programs map[string]Program.Program

func NewPrograms() Programs {
	return Programs(make(map[string]Program.Program))
}

func (p Programs) AddProgram(name Program.Name, program Program.Program) {
	path := programPath(name)
	p[path] = program
}

func programPath(name Program.Name) (path string) {
	if name == Program.DashboardName {
		path = "/"
	} else {
		switch name.Level {
		case Program.SystemLevel:
			path = "/System/" + name.Name
		case Program.ApplicationLevel:
			path = "/Application/" + name.Name
		case Program.UserLevel:
			path = "/User/" + name.Name
		}
	}
	return
}
