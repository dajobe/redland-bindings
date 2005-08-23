//
// EncodingTest.cs: test UTF8 mangling
//
// Author:
//   Edd Dumbill <edd@usefulinc.com>
//


using NUnit.Framework;
using System;
using Redland;

namespace Redland {

	[TestFixture]
	public class EncodingTest {
		
		static String [] unis = { "日本語", "mañana", "현재위치", "hello" };

		[SetUp]
		public void GetReady ()
		{
			
		}

		[Test]
		public void RoundTrip ()
		{
			foreach (String s in unis)
			{
				IntPtr d = Redland.Util.StringToHGlobalUTF8 (s);
				String e = Redland.Util.UTF8PtrToString (d);
				NUnit.Framework.Assert.IsTrue (s.Equals (e), "Strings should match");
			}
		}
	}
}
