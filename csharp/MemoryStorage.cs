//
// MemoryStorage.cs: Redland Statement MemoryStorage class
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

	public class MemoryStorage : Storage {

		public MemoryStorage ()
			: base (null, "memory", null)
		{			
		}

		public MemoryStorage (string options)
			: base (null, "memory", options)
		{
		}
	}
}
