#
# SQL Export
# Created by Querious (1048)
# Created: 7 сентября 2016 г., 10:10:22 GMT+5
# Encoding: Unicode (UTF-8)
#


CREATE DATABASE IF NOT EXISTS `tests` DEFAULT CHARACTER SET utf8 DEFAULT COLLATE utf8_general_ci;
USE `tests`;




SET @PREVIOUS_FOREIGN_KEY_CHECKS = @@FOREIGN_KEY_CHECKS;
SET FOREIGN_KEY_CHECKS = 0;


CREATE TABLE `assignment` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT,
  `product_id` int(11) unsigned DEFAULT NULL,
  `participant_id` int(11) unsigned DEFAULT NULL,
  `ts` timestamp NULL DEFAULT CURRENT_TIMESTAMP,
  `action` enum('c','m') DEFAULT 'c',
  PRIMARY KEY (`id`),
  UNIQUE KEY `idx_id_action` (`product_id`,`participant_id`,`action`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=14 DEFAULT CHARSET=utf8;


CREATE TABLE `product` (
  `id` int(11) unsigned NOT NULL DEFAULT '0',
  `current_owner_id` int(11) unsigned NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`,`current_owner_id`),
  UNIQUE KEY `idx_id_current_owner_id` (`id`,`current_owner_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;




SET FOREIGN_KEY_CHECKS = @PREVIOUS_FOREIGN_KEY_CHECKS;


DELIMITER //
CREATE DEFINER=`root`@`localhost` PROCEDURE `chains`()
    READS SQL DATA
    DETERMINISTIC
select * 
from `assignment`
order by `product_id` asc, `ts` asc, `action` asc;
//
DELIMITER ;




SET @PREVIOUS_FOREIGN_KEY_CHECKS = @@FOREIGN_KEY_CHECKS;
SET FOREIGN_KEY_CHECKS = 0;


LOCK TABLES `assignment` WRITE;
ALTER TABLE `assignment` DISABLE KEYS;
INSERT INTO `assignment` (`id`, `product_id`, `participant_id`, `ts`, `action`) VALUES 
	(1,1,500,'2016-09-06 18:27:36','m'),
	(2,2,600,'2016-09-06 18:27:47','c'),
	(3,3,700,'2016-09-06 18:27:55','c'),
	(6,1,501,'2016-09-06 18:27:36','c'),
	(7,1,502,'2016-09-06 18:28:36','m'),
	(8,2,601,'2016-09-06 18:27:47','m'),
	(9,2,602,'2016-09-06 18:29:13','m'),
	(10,3,701,'2016-09-06 18:44:37','m'),
	(11,4,800,'2016-09-06 18:46:34','c'),
	(12,4,801,'2016-09-06 18:46:34','m'),
	(13,4,802,'2016-09-06 18:46:52','m');
ALTER TABLE `assignment` ENABLE KEYS;
UNLOCK TABLES;


LOCK TABLES `product` WRITE;
ALTER TABLE `product` DISABLE KEYS;
INSERT INTO `product` (`id`, `current_owner_id`) VALUES 
	(1,502),
	(2,602),
	(3,701),
	(4,802);
ALTER TABLE `product` ENABLE KEYS;
UNLOCK TABLES;




SET FOREIGN_KEY_CHECKS = @PREVIOUS_FOREIGN_KEY_CHECKS;


