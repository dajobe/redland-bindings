//
// Node.cs: Redland Node class.
//
// Author:
//	Cesar Lopez Nataren (cesar@ciencias.unam.mx)
//
// (C) 2004, Cesar Lopez Nataren
//

using System;
using System.Runtime.InteropServices;

namespace Rdf {

	public class Node : IWrapper {
		
		IntPtr node;

		public IntPtr Handle {
			get { return node; }
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_node (IntPtr world);

		public Node ()
		{
			node = librdf_new_node (Redland.World.Handle);
		}

		public Node (string uri)
			: this (Redland.World, uri)
		{
		}
		
		public Node (string s, string xml_language, int is_wf_xml)
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
		static extern int librdf_node_equals (IntPtr first_node, IntPtr second_node);
		
		public override bool Equals (object o)
		{
			if(o == null)
 				return false;

			int i = librdf_node_equals (node, ((Node) o).Handle);
			if (i == 0)
				return false;
			else
				return true;
		}

		public static bool operator == (Node n1, Node n2)
		{
			return n1.Equals (n2);
		}

		public static bool operator != (Node n1, Node n2)
		{
			return !n1.Equals (n2);
		}

		public override int GetHashCode ()
		{
			return this.ToString ().GetHashCode ();
		}
		
		[DllImport ("librdf")]
		static extern IntPtr librdf_new_node_from_uri_string (IntPtr world, IntPtr uri);

		private Node (World world, string uri)
		{
			IntPtr iuri = Marshal.StringToHGlobalAuto (uri);
			node = librdf_new_node_from_uri_string (world.Handle, iuri);
                        Marshal.FreeHGlobal (iuri);
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_new_node_from_literal (IntPtr world, IntPtr s, IntPtr xml_language, int is_wf_xml);

		private Node (World world, string s, string xml_language, int is_wf_xml)
		{
			IntPtr istr = Marshal.StringToHGlobalAuto (s);
			IntPtr ilang = Marshal.StringToHGlobalAuto (xml_language);
			node = librdf_new_node_from_literal (world.Handle, istr, ilang, is_wf_xml);
                        Marshal.FreeHGlobal (istr);
                        Marshal.FreeHGlobal (ilang);
		}
		
		[DllImport ("librdf")]
		static extern IntPtr librdf_new_node_from_node (IntPtr node);

		public Node (Node node)
		{
			this.node = librdf_new_node_from_node(node.node);
		}

		internal Node (IntPtr node)
		{
			this.node = node;
		}

		[DllImport ("librdf")]
		static extern void librdf_node_print (IntPtr node, IntPtr fh);

		public void Print (IntPtr fh)
		{
			Console.WriteLine (node == IntPtr.Zero);
			librdf_node_print (node, fh);
		}

		[DllImport ("librdf")]
		static extern IntPtr librdf_node_to_string (IntPtr node);

		public override string ToString ()
		{
			IntPtr istr=librdf_node_to_string (node);
                        return Marshal.PtrToStringAuto(istr);
		}
	}
}
