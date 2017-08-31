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
	"testing"

	"CRW/RT/Program"
)

func TestRoute(t *testing.T) {
	var (
		dashboardProgram          = new(Program.View)
		systemVersionProgram      = new(Program.View)
		applicationVersionProgram = new(Program.View)
		userVersionProgram        = new(Program.View)
		found                     Program.Program
	)

	programs := NewPrograms()
	programs.AddProgram(Program.DashboardName, dashboardProgram)
	programs.AddProgram(Program.Name{Program.SystemLevel, "Version"}, systemVersionProgram)
	programs.AddProgram(Program.Name{Program.ApplicationLevel, "Version"}, applicationVersionProgram)
	programs.AddProgram(Program.Name{Program.UserLevel, "Version"}, userVersionProgram)

	found, _ = programs["/"]
	if found != dashboardProgram {
		t.Errorf("Did not register correct program at /: %v", programs)
	}

	found, _ = programs["/System/Version"]
	if found != systemVersionProgram {
		t.Errorf("Did not register correct program at /System/Version: %v", programs)
	}

	found, _ = programs["/Application/Version"]
	if found != applicationVersionProgram {
		t.Errorf("Did not register correct program at /Application/Version: %v", programs)
	}

	found, _ = programs["/User/Version"]
	if found != userVersionProgram {
		t.Errorf("Did not register correct program at /User/Version: %v", programs)
	}
}
