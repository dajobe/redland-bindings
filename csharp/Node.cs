//
// Node.cs: Redland Node class.
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

	public enum NodeType {
		Unknown,
   		Resource,
		Literal,
	   	Reserved1,
	   	Blank
	};
	
	public class Node : IWrapper, IDisposable {
		
		IntPtr node = IntPtr.Zero;

		bool disposed = false;

		public IntPtr Handle {
			get { return node; }
		}

		[DllImport ("librdf")]
		static extern int librdf_node_get_type (IntPtr node);

		public NodeType Type {
			get {
				return (NodeType) librdf_node_get_type (node);
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
		static extern IntPtr librdf_node_get_literal_value (IntPtr node);

		public string Literal {
			get {
				if (! IsLiteral ())
					throw new RedlandError 
						("Can't get literal value of non-literal");
				IntPtr istr = librdf_node_get_literal_value (node);
				return Marshal.PtrToStringAuto (istr);
			}
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_node_get_literal_value_language (IntPtr node);

		public string Language {
			get {
				if (! IsLiteral ())
					throw new RedlandError 
						("Can't get language of non-literal");
				IntPtr istr = librdf_node_get_literal_value_language (node);
				return Marshal.PtrToStringAuto (istr);
			}
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_node (IntPtr world);

		public Node ()
		{
			node = librdf_new_node (Redland.World.Handle);
		}

		public Node (System.Uri uri)
			: this (Redland.World, uri)
		{
		}
		
		public Node (string s)
			: this (Redland.World, s, null, false)
		{
		}

		public Node (string s, string xml_language, bool is_wf_xml)
			: this (Redland.World, s, xml_language, is_wf_xml)
		{
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_node_from_uri (IntPtr world, IntPtr uri);

		public Node (Uri uri)
		{
 			// Console.WriteLine ("Making Node from Uri {0} with handle {1}", uri.ToString (), uri.Handle);
			node = librdf_new_node_from_uri (Redland.World.Handle, uri.Handle);
		}

		[DllImport ("librdf")]
		static extern void librdf_free_node (IntPtr node);

		protected void Dispose (bool disposing)
		{
			if (! disposed) {
				// only dispose of managed objects if
				// disposing is true.

				if (node != IntPtr.Zero) {
					librdf_free_node (node);
					node = IntPtr.Zero;
				}
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
		static extern int librdf_node_equals (IntPtr first_node, IntPtr second_node);
		
		public override bool Equals (object o)
		{
			if (o == null)
 				return false;

			int i = librdf_node_equals (node, ((Node) o).Handle);
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
		static extern IntPtr librdf_new_node_from_uri_string (IntPtr world, IntPtr uri);

		private Node (World world, System.Uri uri)
		{
			IntPtr iuri = Marshal.StringToHGlobalAuto (uri.ToString());
			node = librdf_new_node_from_uri_string (world.Handle, iuri);
			Marshal.FreeHGlobal (iuri);
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_node_from_literal (IntPtr world, IntPtr s, IntPtr xml_language, int is_wf_xml);

		private Node (World world, string s, string xml_language, bool is_wf_xml)
		{
			IntPtr istr = Marshal.StringToHGlobalAuto (s);
			IntPtr ilang = Marshal.StringToHGlobalAuto (xml_language);
			int is_xml = is_wf_xml ? 1: 0;
			node = librdf_new_node_from_literal (world.Handle, istr, ilang, is_xml);
			Marshal.FreeHGlobal (istr);
			Marshal.FreeHGlobal (ilang);
		}
		
		[DllImport ("librdf")]
		static extern IntPtr librdf_new_node_from_node (IntPtr node);

		public Node (Node node)
		{
			this.node = librdf_new_node_from_node (node.node);
		}

		internal Node (IntPtr node)
		{
			this.node = librdf_new_node_from_node (node);
		}

		[DllImport ("librdf")]
		static extern void librdf_node_print (IntPtr node, IntPtr fh);

		public void Print (IntPtr fh)
		{
			if (node == IntPtr.Zero)
				Util.fputs ("null", fh);
			else
				librdf_node_print (node, fh);
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_node_to_string (IntPtr node);

		public override string ToString ()
		{
			IntPtr istr = librdf_node_to_string (node);
			return Marshal.PtrToStringAuto (istr);
		}
		
		[DllImport ("librdf")]
		static extern IntPtr librdf_node_get_uri (IntPtr node);

		public Redland.Uri Uri {
			get {
				if (! IsResource ())
					throw new RedlandError ("Can't get URI of non-resource");
				return new Redland.Uri (librdf_node_get_uri (node));
			}
		}
	}
}
