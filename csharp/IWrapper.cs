//
// IWrapper.cs: Redland Iterator wrapper class
//
// $Id$
//
// Author:
//	Cesar Lopez Nataren (cesar@ciencias.unam.mx)
//
// (C) 2004, Cesar Lopez Nataren 
//

using System;
using System.Runtime.InteropServices;

namespace Redland {

	public interface IWrapper {		

		HandleRef Handle { get; }
	}
}
