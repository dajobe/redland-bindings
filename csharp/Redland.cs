//
// World.cs: Redland Initialization class
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

	internal class Redland {
		
		internal static World World;

		static Redland ()
		{
			World = new World ();
			World.Open ();
		}
	}

}
