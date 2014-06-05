<?php
/* @var $user \application */
?>
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
    <head runat="server">
        <title></title>
        <link rel="stylesheet" media="all" href="review.css" />
        <link rel="stylesheet" href="//code.jquery.com/ui/1.10.4/themes/smoothness/jquery-ui.css" />
        <script src="//code.jquery.com/jquery-1.10.2.js"></script>
        <script src="//code.jquery.com/ui/1.10.4/jquery-ui.js"></script>
    </head>
    <body>
        <div style="display: inline-block;width:100%">
            <a href="review.php" style="position:absolute;top:20px;left:-20%;">
                <img width="50" height="50" src="imgs/left.png" />
            </a>
            <div>
                <h1 class="header" style="display: inline-block">Gennemgå ansøgning</h1>
                <img style="display: inline-block; right: 0px; margin-top: -25px; position: absolute; margin-right: -22px" src="imgs/logo.png" />
            </div>
            <hr />

            <div class="box" style="float: left;">
                <h1 class="header">Info og kompentancer</h1>
                <label>Navn</label>
                <p class="note"><?php echo $user->getName(); ?></p>
                <label>Alder</label>
                <p class="note"><?php echo "20" ?></p>
                <h2>Kontakt info</h2> 
                <label>Telefon nummer</label>
                <p class="note"><?php echo $user->getTelephone(); ?></p>
                <label>Email</label>
                <p class="note"><?php echo $user->getEmail(); ?></p>
                <label>Addresse</label>
                <p class="note"><?php echo $user->getAddress(); ?></p>
                <label>Kommune / by</label>
                <p class="note"><?php echo $user->getCity(); ?></p>

                <h2>Skole og arbejde</h2>
                <label>Tidligere skole</label>
                <p class="note"><?php echo $user->getSchool(); ?></p>
                <label>Tidligere jobs</label>
                <p class="note"><?php echo $user->getEarlierjobs(); ?></p>
                <h2>Tidligere medarbejder hos arla ?</h2>
                <?php
                if ($user->getPrevarla()):
                    ?>
                    <p class="note">Ja</p>
                <?php else: ?>
                    <p class="note">Nej</p>
                <?php endif; ?>




                <hr />
            </div>
            <div class="box" style="float: left;">
                <h1 class="header">Kommentar</h1>
                <form>
                    <label>Kommentar til personen</label><br />
                    <textarea id="commentArea" class="commentBox note" name="commentArea"><?php echo $user->getComment(); ?></textarea>
                    <br />
                    <label>Dine initialer</label><br />
                    <input class="note" type="text" value="" />
                    <br />
                    <input type="submit" class="arlabtn" value="Gem kommentar" />
                </form>
                <hr />
            </div>
            <div class="line" style="width: 100%; top: 20px;;height:auto;">
                <div class="box" style="float: left;">
                    <h1 class="header">Om</h1>
                    <label>Personlig beskrivelse</label>
                    <p class="note"><?php echo $user->getPersonaldescription(); ?></p>
                    <label>Ansættelses grund</label>
                    <p class="note"><?php echo $user->getHirereason(); ?></p>
                    <hr />
                </div>
                <div class="box" style="float: left">
                    <h1 class="header">Prioitering</h1>
                    <form>
                        <label>Prioitering</label><br />
                        <br />
                        <select>
                            <option>Høj
                            </option>
                            <option>Mellem
                            </option>
                            <option>lav
                            </option>
                        </select>
                        <br />
                        <label>Dine initialer</label><br />
                        <input class="note" name="prio-inital" type="text" value="" />
                        <br />
                        <input type="submit" class="arlabtn" value="Gem valg" />
                    </form>
                    <hr />
                </div>
            </div>
        </div>
    </body>
</html>
