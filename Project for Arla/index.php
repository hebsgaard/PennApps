<?php

//this is the main index of the api builder tool


include_once '../api/includer.php';
api\loadAll();

include_once 'JavaCodeGenerator.php';
include_once 'PhpCodeGenerator.php';
include_once 'IosGenerator.php';
include_once 'MysqlIntegration.php';
include_once 'MysqlGenerator.php';
include_once 'CodeStructures.php';
include_once 'CodeClassGenerator.php';
include_once 'DoxygenCommentGenerator.php';

$propertyGenerator = new simpleDataModelCodeJsonTransport();


//PERSON
//$propertyGenerator->addModel("id", "int");
//$propertyGenerator->addModel("name", "String");
//$propertyGenerator->addModel("age", "int");
//$propertyGenerator->addModel("gender", "int");
//$propertyGenerator->addModel("password", "String");
//$propertyGenerator->addModel("email", "String");
//$propertyGenerator->addModel("club", "int");
//$propertyGenerator->addModel("teamInClub", "int");
//$propertyGenerator->addModel("roleInClub", "int");
//CLUB
//$propertyGenerator->addModel("id", "int");
//$propertyGenerator->addModel("name", "String");
//$propertyGenerator->addModel("address", "String");
//$propertyGenerator->addModel("homepage", "String");
//$propertyGenerator->addModel("facebook","String");
//NEWS
/* $propertyGenerator->addModel("id", "int");
  $propertyGenerator->addModel("createdAt", "String");
  $propertyGenerator->addModel("createdBy", "String");
  $propertyGenerator->addModel("headline", "String");
  $propertyGenerator->addModel("text", "String");
  $propertyGenerator->addModel("clubId", "int");
  $propertyGenerator->addModel("teamId", "int"); */



/**
 * arla.
 * 
 */
$propertyGenerator->addModel("id", "int");
$propertyGenerator->addModel("createdAt", "String");
$propertyGenerator->addModel("name", "String");
$propertyGenerator->addModel("address", "String");
$propertyGenerator->addModel("postal", "String");
$propertyGenerator->addModel("city", "String");
$propertyGenerator->addModel("telephone", "String");
$propertyGenerator->addModel("email", "String");
$propertyGenerator->addModel("munipicial", "String");
$propertyGenerator->addModel("smoker", "int");
$propertyGenerator->addModel("cpr", "String");
$propertyGenerator->addModel("children", "String");
$propertyGenerator->addModel("civilstatus", "String");
$propertyGenerator->addModel("school", "String");
$propertyGenerator->addModel("earlierjobs", "String");
$propertyGenerator->addModel("truckcert", "Boolean");
$propertyGenerator->addModel("cleancert", "Boolean");
$propertyGenerator->addModel("knowsap", "Boolean");
$propertyGenerator->addModel("knowlean", "Boolean");
$propertyGenerator->addModel("knowenglish", "Boolean");
$propertyGenerator->addModel("knowenglishtext", "Boolean");
$propertyGenerator->addModel("personaldescription", "String");
$propertyGenerator->addModel("hirereason", "String");
$propertyGenerator->addModel("health", "String");
$propertyGenerator->addModel("readydate", "String");
$propertyGenerator->addModel("worktype", "String");
$propertyGenerator->addModel("day", "Boolean");
$propertyGenerator->addModel("night", "Boolean");


$propertyGenerator->addModel("prevarla", "Boolean");
$propertyGenerator->addModel("prevarlawhere", "String");
$propertyGenerator->addModel("prevarlawhat", "String");
$propertyGenerator->addModel("lastredone", "String");
$propertyGenerator->addModel("comment", "String");





$clsName = "application";

$text = $propertyGenerator->useGenerator(new PhpCodeGenerator(), $clsName);
printCode($text, "php");

$text = $propertyGenerator->useGenerator(new JavaCodeGenerator(), $clsName);

printCode($text, "java");

$text = $propertyGenerator->useGenerator(new IosCodeGenerator(), $clsName);

printCode($text, "IOS");

$text = $propertyGenerator->useGenerator(new MysqlInsertGenerator(), $clsName);

printCode($text, "mysql insert(php flavored)");

$text = $propertyGenerator->useGenerator(new MysqlLoadGenerator(), $clsName);

printCode($text, "mysql load (php flavored)");

$text = $propertyGenerator->useGenerator(new \MysqltableGenerator(), $clsName);

printCode($text, "mysql create(php flavored)");


$arr = $propertyGenerator->getModels();
$str = "";
foreach ($arr as $a) {
    $name = $a[0];
    $type = $a[1];
    $str.="<p>$name</p><input type=\"text\" name=\"$name\"/>\r\n";
}
\printCode($str, "html form");

function printCode($text, $type) {
    echo "<h1>$type</h1><hr>";
    $text = \str_replace("\t", "&nbsp;&nbsp;&nbsp;&nbsp;", $text);
    echo \nl2br($text);
    echo "<hr>";
}
