import UK.ac.bristol.ilrt.redland.*;

class test {
  
  public static void main(String[] args) {
    byte[] storage_name=null;
    byte[] name=null;
    byte[] options_string=null;
    
    UK.ac.bristol.ilrt.redland.redland.init_world(name, null);

    System.out.println("Version is '" + UK.ac.bristol.ilrt.redland.redland.version + "'\n");
    System.out.println("Copyright is '" + UK.ac.bristol.ilrt.redland.redland.copyright + "'\n");

    storage_name=new String("memory").getBytes();
    UK.ac.bristol.ilrt.redland.redland.new_storage(storage_name,
                                                   name,
                                                   options_string);

    
    UK.ac.bristol.ilrt.redland.redland.destroy_world();
    
  }
}
