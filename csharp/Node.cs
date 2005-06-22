//
// Node.cs: Redland Node class.
//
// $Id$
//
// Author:
//	Cesar Lopez Nataren (cesar@ciencias.unam.mx)
//  Edd Dumbill (edd@usefulinc.com)
//
// (C) 2004, Cesar Lopez Nataren, Edd Dumbill
//

using System;
using System.Runtime.InteropServices;

namespace Redland {

	public enum NodeType {
		Unknown,
   		Resource,
		Literal,
	   	Reserved1,
	   	Blank
	};
	
	public class Node : IWrapper, IDisposable {
		
		private HandleRef handle;

		private bool disposed = false;
		private World world = Redland.World.AddReference ();

		public HandleRef Handle {
			get { return handle; }
		}

		[DllImport ("librdf")]
		static extern int librdf_node_get_type (HandleRef node);

		public NodeType Type {
			get {
				return (NodeType) librdf_node_get_type (handle);
			}
		}
		
		public bool IsResource ()
		{
			return Type == NodeType.Resource;
		}

		public bool IsLiteral ()
		{
			return Type == NodeType.Literal;
		}

		public bool IsBlank ()
		{
			return Type == NodeType.Blank;
		}		

		[DllImport ("librdf")]
		static extern IntPtr librdf_node_get_literal_value (HandleRef node);

		public string Literal {
			get {
				if (! IsLiteral ())
					throw new RedlandError 
						("Can't get literal value of non-literal");
				IntPtr istr = librdf_node_get_literal_value (handle);
				return Util.UTF8PtrToString (istr);
			}
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_node_get_literal_value_language (HandleRef node);

		public string Language {
			get {
				if (! IsLiteral ())
					throw new RedlandError 
						("Can't get language of non-literal");
				IntPtr istr = librdf_node_get_literal_value_language (handle);
				return Util.UTF8PtrToString (istr);
			}
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_node (HandleRef world);

		public Node ()
		{
			IntPtr node = librdf_new_node (world.Handle);
			handle = new HandleRef (this, node);
		}

		public Node (string s)
			: this (s, null, false)
		{
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_node_from_uri (HandleRef world, HandleRef uri);
		public Node (Uri uri)
		{
 			// Console.WriteLine ("Making Node from Uri {0} with handle {1}", uri.ToString (), uri.Handle);
			IntPtr node = librdf_new_node_from_uri (world.Handle, uri.Handle);
			handle = new HandleRef (this, node);
		}

		[DllImport ("librdf")]
		static extern void librdf_free_node (HandleRef node);

		protected void Dispose (bool disposing)
		{
			if (! disposed) {
				// only dispose of managed objects if
				// disposing is true.

				if (handle.Handle != IntPtr.Zero) {
					librdf_free_node (handle);
					handle = new HandleRef (this, IntPtr.Zero);
				}
				world.RemoveReference ();
				world = null;
				disposed = true;
			}
		}

		public void Dispose ()
		{
			Dispose (true);
			GC.SuppressFinalize (this);
		}

		~Node ()
		{
			Dispose (false);
		}

		[DllImport ("librdf")]
		static extern int librdf_node_equals (HandleRef first_node, HandleRef second_node);
		
		public override bool Equals (object o)
		{
			if (o == null)
 				return false;

			int i = librdf_node_equals (handle, ((Node) o).Handle);
			if (i == 0)
				return false;
			else
				return true;
		}

		public static bool operator == (Node n1, Node n2)
		{

			if (Object.Equals (n1, null))
				if (Object.Equals (n2, null))
					return true;
				else
					return false;

			return n1.Equals (n2);
		}

		public static bool operator != (Node n1, Node n2)
		{
			if (Object.Equals (n1, null))
				if (Object.Equals (n2, null))
					return false;
				else
					return true;

			return !n1.Equals (n2);
		}

		public override int GetHashCode ()
		{
			return this.ToString ().GetHashCode ();
		}
		
		[DllImport ("librdf")]
		static extern IntPtr librdf_new_node_from_uri_string (HandleRef world, IntPtr uri);

		public Node (System.Uri uri)
		{
			IntPtr iuri = Util.StringToHGlobalUTF8 (uri.ToString());
			IntPtr node = librdf_new_node_from_uri_string (world.Handle, iuri);
			Marshal.FreeHGlobal (iuri);
			handle = new HandleRef (this, node);
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_node_from_literal (HandleRef world, IntPtr s, IntPtr xml_language, int is_wf_xml);

		public Node (string s, string xml_language, bool is_wf_xml)
		{
			IntPtr istr = Util.StringToHGlobalUTF8 (s);
			IntPtr ilang = Util.StringToHGlobalUTF8 (xml_language);
			int is_xml = is_wf_xml ? 1: 0;
			IntPtr node = librdf_new_node_from_literal (world.Handle, istr, ilang, is_xml);
			handle = new HandleRef (this, node);
			Marshal.FreeHGlobal (istr);
			Marshal.FreeHGlobal (ilang);
		}
		
		[DllImport ("librdf")]
		static extern IntPtr librdf_new_node_from_node (HandleRef node);

		public Node (Node node)
		{
			IntPtr newnode = librdf_new_node_from_node (node.handle);
			handle = new HandleRef (this, newnode);
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_node_from_node (IntPtr node);

		internal Node (IntPtr node)
		{
			IntPtr newnode = librdf_new_node_from_node (node);
			handle = new HandleRef (this, newnode);
		}

		[DllImport ("librdf")]
		static extern void librdf_node_print (HandleRef node, IntPtr fh);

		public void Print (IntPtr fh)
		{
			if (handle.Handle == IntPtr.Zero)
				Util.fputs ("null", fh);
			else
				librdf_node_print (handle, fh);
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_node_to_string (HandleRef node);

		public override string ToString ()
		{
			IntPtr istr = librdf_node_to_string (handle);
			return Util.UTF8PtrToString (istr);
		}
		
		[DllImport ("librdf")]
		static extern IntPtr librdf_node_get_uri (HandleRef node);

		public Uri Uri {
			get {
				if (! IsResource ())
					throw new RedlandError ("Can't get URI of non-resource");
				return new Uri (librdf_node_get_uri (handle));
			}
		}
	}
}
