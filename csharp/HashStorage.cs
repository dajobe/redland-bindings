//
// HashStorage.cs:
//
// Author:
//	Cesar Lopez Nataren (cesar@ciencias.unam.mx)
//
// (C) 2004, Cesar Lopez Nataren
//

using System;
using System.Runtime.InteropServices;

namespace Rdf {

	public class HashStorage : Storage {
		
		public HashStorage (string hash_name, string options)
			: base ("hashes", hash_name, options)
		{			
		}
	}
}
