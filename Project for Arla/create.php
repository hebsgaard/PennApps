<?php
include_once 'BaseRequestResponse.php';
include_once 'settings.php';
include_once '../api/includer.php';
\api\loadAll();
$myResult = $mysqli->query("CREATE TABLE IF NOT EXISTS `application` ( `id` int NOT NULL AUTO_INCREMENT PRIMARY KEY,`createdAt` text,`name` text,`address` text,`postal` text,`city` text,`telephone` text,`email` text,`munipicial` text,`smoker` int,`school` text,`earlierjobs` text,`truckcert` tinyint,`cleancert` tinyint,`knowsap` tinyint,`knowlean` tinyint,`knowenglish` tinyint,`knowenglishtext` tinyint,`personaldescription` text,`hirereason` text,`readydate` text,`worktype` text,`day` tinyint,`night` tinyint,`prevarla` tinyint,`prevarlawhere` text,`prevarlawhat` text,`lastredone` text,`comment` text );");
return $myResult;
?>