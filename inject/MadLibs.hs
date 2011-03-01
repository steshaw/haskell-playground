-- 
-- Port of MadLibs.java from http://code.google.com/p/guice-examples/.
--
-- http://code.google.com/p/guice-examples/source/browse/trunk/GuiceProject/src/propertyfile/impl/MadLibs.java
--

module MadLibs (
  joke
) where

joke :: String -> String -> String -> String
joke name verb noun = "One day, " ++ name ++ " " ++ verb ++ " to New York to see the " ++ noun ++ "."

{-
import com.google.inject.Inject;
import com.google.inject.name.Named;

public class MadLibs {
  private final String name;
  private final String verb;
  private final String noun;

  @Inject
  public MadLibs(@Named("nameOfPerson")String name, @Named("pastTenseVerb") String verb, @Named("noun")String noun) {
    this.name = name;
    this.verb = verb;
    this.noun = noun;
  }

  public String getJoke() {
    return "One day, " + name + " " + verb + " to New York to see the " + noun + ".";
  }

}
-}
