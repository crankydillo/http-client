import sbt._

import java.io._

class Tools(info: ProjectInfo) extends DefaultProject(info) {
  val mavenLocal = "Local Maven Repository" at 
  "file://"+Path.userHome+"/.m2/repository"

  lazy val mkdir = task {args => 
    task {
      val path = args(0);
      mkDirs(path.split("/").toList)
      None
    }
  }

  lazy val mkJavaClass = task {args =>
    task {
      if (args.size < 2)
        println("Specify class name and package # (list-java-pkgs for list)");
      else {
        val className = args(0);
        val pkgId = args(1).toInt;
        mkJavaClassFn(className, pkgId);
      }
      None
    }
  }

  lazy val mkJavaAndSpec = task {args =>
    task {
      if (args.size < 2)
        println("Specify class name and package # (list-java-pkgs for list)");
      else {
        val className = args(0);
        val pkgId = args(1).toInt;

        mkJavaClassFn(className, pkgId);

        val base = new File("src/main/java");
        val (_, pkgName) = packages(base)(pkgId - 1);
        val lst = pkgName.split("\\.");
        val specDir = mkDirs(List("src", "test", "scala") ++ lst)
        mkSpecFn(className + "Spec", pkgName, specDir)
      }
      None
    }
  }

  lazy val mkScalaAndSpec = task {args =>
    task {
      if (args.size < 2)
        println("Specify class name and package # (list-scala-pkgs for list)");
      else {
        val className = args(0);
        val pkgId = args(1).toInt;

        mkScalaClassFn(className, pkgId);

        val base = new File("src/main/scala");
        val (_, pkgName) = packages(base)(pkgId - 1);
        val lst = pkgName.split("\\.");
        val specDir = mkDirs(List("src", "test", "scala") ++ lst)
        mkSpecFn(className + "Spec", pkgName, specDir)
      }
      None
    }
  }


  lazy val mkJavaAndTest = task {args =>
    task {
      if (args.size < 2)
        println("Specify class name and package # (list-java-pkgs for list)");
      else {
        val className = args(0);
        val pkgId = args(1).toInt;

        mkJavaClassFn(className, pkgId);

        val base = new File("src/main/java");
        val (_, pkgName) = packages(base)(pkgId - 1);
        val lst = pkgName.split("\\.");
        val testDir = mkDirs(List("src", "test", "java") ++ lst)
        mkUnitTestFn(className + "Test", pkgName, testDir)
      }
      None
    }
  }


  private def mkJavaClassFn(className: String, pkgId: Int): File = {
    val base = new File("src/main/java");
    val (pkgDir, pkgName) = packages(base)(pkgId - 1);
    val javaFile = new File(pkgDir, className + ".java");
    FileIO.write(javaFile, Templates.javaClass(className, pkgName));
    javaFile
  }

  private def mkScalaClassFn(className: String, pkgId: Int): File = {
    val base = new File("src/main/scala");
    val (pkgDir, pkgName) = packages(base)(pkgId - 1);
    val scalaFile = new File(pkgDir, className + ".scala");
    FileIO.write(scalaFile, Templates.scalaClass(className, pkgName));
    scalaFile
  }

  private def mkSpecFn(
      specName: String
      , pkgName: String
      , specDir: File
  ): File = {
    val specFile = new File(specDir, specName + ".scala");
    FileIO.write(specFile, Templates.spec(specName, pkgName));
    specFile
  }

  private def mkUnitTestFn(
      testName: String
      , pkgName: String
      , testDir: File
  ): File = {
    val testFile = new File(testDir, testName + ".java");
    FileIO.write(testFile, Templates.unitTest(testName, pkgName));
    testFile
  }


  lazy val mkScalaClass = task {args =>
    task {
      if (args.size < 2)
        println("Specify class name and package # (list-java-pkgs for list)");
      else {
        val className = args(0);
        val pkgId = args(1).toInt;
        mkScalaClassFn(className, pkgId);
      }
      None
    }
  }

  lazy val mkJavaPkg = task {args =>
    task {
      val pkgName = args(0);
      val lst = pkgName.split("\\.");
      mkDirs(List("src", "main", "java") ++ lst)
      mkDirs(List("src", "test", "java") ++ lst)
      None
    }
  }

  lazy val mkScalaPkg = task {args =>
    task {
      val pkgName = args(0);
      val lst = pkgName.split("\\.");
      mkDirs(List("src", "main", "scala") ++ lst)
      mkDirs(List("src", "test", "scala") ++ lst)
      None
    }
  }

  lazy val listJavaPkgs = task {
    printPackages(new File("src/main/java"));
    None
  }

  lazy val listScalaPkgs = task {
    printPackages(new File("src/main/scala"));
    None
  }

  private def mkDirs(names: List[String]): File = {
    val path = names.mkString("/");
    val f = new File(path);
    if (!f.exists)
      f.mkdirs();
    f
    /*
    else
      println(path + " already exists.");
    */
  }

  private def printPackages(baseDir: File): Unit = {
    packages(baseDir)
    .zipWithIndex
    .foreach {p => println((p._2 + 1) + ". " + p._1._2)}
  }

  private def packages(baseDir: File): List[(File, String)] = {
    import scala.collection.mutable.ArrayBuffer

    def packages_h(dir: File
        , parents: List[File]
        , result: ArrayBuffer[(File, String)]
    ): Unit = {

      var pair = 
        if (parents.size > 1) {
          result += 
            (
              dir
              , parents.drop(1).map(_.getName).mkString(".") + "." + dir.getName
            )
        } else if (parents.size == 1) {
          result += (dir, dir.getName)
        }

      dir
      .listFiles
      .filter {_.isDirectory}
      .foreach {
        packages_h(
            _
            , parents + dir
            , result
          )
      }
    }

    val r = new ArrayBuffer[(File, String)]();
    packages_h(baseDir, Nil, r);
    r.toList
  }
}

object Templates {
  def javaClass(className: String, packageName: String): String = {
    Copyright +
    "package " + packageName + ";\n\n" +
    "public class " + className + " {\n\n}"
  }

  def scalaClass(className: String, packageName: String): String = {
    Copyright +
    "package " + packageName + "\n\n" +
    "class " + className + " {\n\n}"
  }

  def spec(specName: String, packageName: String): String = {
    Copyright +
    "package " + packageName + "\n\n" +
    "import org.specs._\n" +
    "import org.specs.runner.JUnit4\n\n" +
    "class " + specName + "Test extends JUnit4(" + specName + ")\n" +
    "object " + specName + " extends Specification {\n\n" +
    "  \"Something\" should {\n" +
    "    \"do something\" in {\n" +
    "    }\n" +
    "  }\n" +
    "}" 
  }

  def unitTest(testName: String, packageName: String): String = {
    Copyright +
    "package " + packageName + ";\n\n" +
    "import org.junit.*;\n" +
    "import static org.junit.Assert.*;\n\n" + 
    "public class " + testName + " {\n\n" +
    "    @Test\n" +
    "    public static void aTest() {\n" +
    "        assertTrue(false);\n" +
    "    }\n\n\n" +
    "    @BeforeClass\n" + 
    "    public static void oneTimeSetup() {\n" +
    "    }\n\n" +
    "    @AfterClass\n" + 
    "    public static void onTimeTearDown() {\n" +
    "    }\n\n" +
    "    @Before\n" +
    "    public void setUp() {\n" +
    "    }\n\n" +
    "    @After\n" +
    "    public void tearDown() {\n" +
    "    }\n\n" +
    "}" 
  }

  lazy val Copyright = 
"""/**
* Copyright 2010 Samuel Cox
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing,
* software distributed under the License is distributed on an
* "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
* KIND, either express or implied. See the License for the
* specific language governing permissions and limitations
* under the License.
*/
""" 
}

object FileIO {
  def write(f: File, data: String): Unit = {
    val writer = new BufferedWriter(new FileWriter(f));
    try {
      writer.write(data);
      writer.flush();
    } finally {
      try { writer.close(); } catch { case e:Exception => {}}
    }
  }
}
