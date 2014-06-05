<?php

include_once "../api/includer.php";
\api\loadAll();

$id = \intval(getGet("delete", -1));
if ($id != -1) {
    include_once 'delete.php';
    \deleteFromDb($id);
    echo "<script>alert(\"Bruger slettet\");</script>";
}

$id = intval(getGet("id", "-1"));
if ($id != -1) {
    showSpecific($id);
} else {
    genList();
}

function genList() {
    $ssap = \getGet("ssap", "0") == 1;
    $truck = \getGet("truck", "0") == 1;
    $hyg = \getGet("hyg", "0") == 1;
    $weekend = \getGet("weekend", "0") == 1;
    $everyday = \getGet("everyday", "0") == 1;
    $vacation = \getGet("vacation", "0") == 1;
    $lean = \getGet("lean", "0") == 1;
    $day = \getGet("day", "0") == 1;
    $night = \getGet("night", "0") == 1;
    $job = \getGet("job", "0") == 1;
    $smoke = \getGet("smoke", "0") == 1;
    $eng = \getGet("eng", "0") == 1;
    $prev = \getGet("prevarla", "0") == 1;
    $date = \getGet("date", "0");
    $freetext = \GetGet("freetext", "");
    include "search.php";
    $where = applySearch($ssap, $truck, $hyg, $weekend, $everyday, $vacation, $lean, $day, $night, $job, $smoke, $eng, $prev, $date, $freetext);


    $elements = include_once 'load.php';
    echo "<ul>";
    foreach ($elements as $ele) {
        /* @var $ele application */
        $name = $ele->getName();
        $id = $ele->getId();
        $prev = "";
        $text = "random random random...";
        if ($ele->getPrevarla()) {
            $prev = "har været ansat hos arla før.";
        } else {
            $prev = "Har ikke været ansat hos arla før";
        }
        $age = "xx år"; //tood change me once sure of this format and alike.
        echo "<li><a class=\"left\" href=\"?id=$id\"><p>$name, $age,$prev,$text </p></a><a class=\"delete\" href=\"?delete=$id\">Slet ansøgning</a></li>";
        // \var_dump($ele);
    }
    echo "</ul>";
    echo "</div></body></html>";
}

function showSpecific($id) {
    $app = include_once 'loadbyid.php';

    if (\count($app) == 1) {

        $user = $app[0];
        /* @var $e \application */


//        echo "<div><h1>Oplysninger om personen</h1>";
//
//        printInfo("Navn", $e->getName());
//        printInfo("Addresse", $e->getAddress());
//        printInfo("By(post nummer)", $e->getCity() . "," . $e->getPostal());
//        printInfo("Kommune", $e->getMunipicial());
//        printInfo("Skole", $e->getSchool());
//        printInfo("Tidliger karriere", $e->getEarlierjobs());
//        printInfo("Email", $e->getEmail());
//        printInfo("Grund til at hyrer", $e->getHirereason());
//        printInfo("Personlig beskrivelse", $e->getPersonaldescription());
//
//        if ($e->getPrevarla()) {
//            echo "<p><b>Har arbejdet hos arla før</b></p>";
//            \printInfo("- ved ", $e->getPrevarlawhere());
//            \printInfo("- med ", $e->getPrevarlawhat());
//        } else {
//            echo "<p> har ikke arbejdet ved arla før</p>";
//        }
//        echo "<h1> Tilføj kommentar (efter møde) her</h1> ";
//        echo "<form  action=\"\" method=\"GET\">";
//        echo "<input type=\"hidden\" name=\"id\" value=\"$id\"/>";
//        echo "<textarea name=\"comment\" >Kommentar her</textarea>";
//
//        echo "<br><input type=\"submit\" value=\"Tilføj kommentar\"/>";
//
//        echo "</form>";
//
//        echo "</div>";
        $html = include 'displayApp.php';
    } else {
        echo "<p>Denne ansøgning findes ikke mere!</p>";
    }
}

function printInfo($label, $text) {
    echo "<p> $label: $text</p>";
}

function applySearch($ssap, $truck, $hyg, $weekend, $everyday, $vacation, $lean, $day, $night, $job, $smoke, $eng, $prev, $date, $freeText) {
    $where = "";
    if ($ssap || $truck || $hyg || $weekend || $everyday || $vacation || $lean || $day || $night || $job || $smoke || $eng || $prev || $date || $freeText) {
        $where = "WHERE ";
        if ($ssap) {
            $where.=" knowsap = 1 AND ";
        }
        if ($truck) {
            $where .=" truckcert = 1 AND ";
        }
        if ($hyg) {
            $where.=" cleancert =1 AND ";
        }
        if ($lean) {
            $where.=" knowlean =1 AND ";
        }
        if ($eng) {
            $where.=" knowenglish =1 AND knowenglishtext = 1 AND ";
        }
        if ($day) {
            $where.=" day =1 AND ";
        }
        if ($night) {
            $where.=" night =1 AND ";
        }
        if ($prev) {
            $where.=" prevarla =1 AND ";
        }
        if ($smoke) {
            $where.=" smoker =1 AND ";
        }
        if ($freeText) {
            $where.=" ((INSTR(`name`,'$freeText')>0) OR (INSTR(`personaldescription`,'$freeText')>0) OR (INSTR(`hirereason`,'$freeText')>0) OR (INSTR(`earlierjobs`,'$freeText')>0) OR (INSTR(`school`,'$freeText')>0)) AND ";
        }

        if ($date != "" && \strlen($date) > 10) { //well has to be the right length: year-mm-dd
            $where.=" readydate <  STR_TO_DATE(\"$date\",\"%Y-%m-%D\") AND ";
        }

        $where.=" 1 = 1";
    }
    return $where;
}

function mc($vl) {
    if ($vl == 1) {
        echo "checked ";
    }
}

function dt($date) {
    if ($date != "") {
        echo $date;
    }
}
