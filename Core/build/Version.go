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

// Level: System
// Name: Version

package CRWSystemVersion

import (
	CRWRTDisplay "CRW/RT/Display"
	CRWRTProgram "CRW/RT/Program"
)

var Program CRWRTProgram.Program = &CRWRTProgram.View{
	Execute: Execute,
}

func Execute(display CRWRTDisplay.Display) error {
	return display.Display("0.0.0")
}
