//
// Errors.cs: Redland Error Handling
//
// Author:
//	Edd Dumbill (edd@usefulinc.com)
//
// (C) 2004 Edd Dumbill
//

using System;

namespace Redland {
	public abstract class RedlandFault : Exception {
		public string [] Errors;
		public RedlandFault (string msg, string [] errs) : base (msg)
		{
			// we have the string array to accumulate a list of
			// errors that can happen during an operation
			Errors = errs;
		}
	}

	public class RedlandError : RedlandFault {
		public RedlandError (string msg, string [] errs): base (msg, errs) { }
	}

}
