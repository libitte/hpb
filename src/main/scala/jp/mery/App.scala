package jp.mery
import scala.util.Try
import scala.collection.JavaConversions._
import org.openqa.selenium.htmlunit.HtmlUnitDriver

object App {

  def main(args: Array[String]){
    val area = List(
      //"http://beauty.hotpepper.jp/svcSA/macAD/salon/",//渋谷
      //"http://beauty.hotpepper.jp/svcSA/macAA/salon/",//新宿
      //"http://beauty.hotpepper.jp/svcSA/macAB/salon/" //池袋
      "http://beauty.hotpepper.jp/svcSA/macAC/salon/sacX011/" //六本木・麻布・赤坂
    )
    area.par map crawl
  }

//crawler

  def crawl(url:String){
    (getShopLinks(url) map getDetail) map printShop
    next(url) match {
      case Some(url) => crawl(url)
      case _ => //end
    }
  }

  def getShopLinks(url:String):List[String] = {
    findAttr(url,"//h3[@class='slcHead cFix']/a")
  }

  def getDetail(url:String):ShopData = {
    val name = getText(url,"//em[@class='seoCaption']")
    val address = getText(url,"//div[@id='mainContents']//th[text()='住所']/../td")
    val phone = getPhoneNumber(url)
    ShopData(name,address,phone)
  }

  def getPhoneNumber(url:String):Option[String] = {
    val link = findAttr(url,"//div[@id='mainContents']//th[text()='電話番号']/../td/a").headOption
    link match {
      case Some(link) => getText(link,"//td")
      case _ => None
    }
  }

  def next(url:String):Option[String] = {
    findAttr(url,"//li[@class='pa top0 right0 afterPage']/a").headOption
  }

  //shop

  case class ShopData (
    name:Option[String],
    address:Option[String],
    phone:Option[String]
  )

  def printShop(shop:ShopData){
    def safePrint(str:Option[String]){
      print(str.getOrElse("取得できませんでした").replace("\'", "").replace("\"", "").replace(",", ""))
    }
    safePrint(shop.name)
    print(",")
    safePrint(shop.address)
    print(",")
    safePrint(shop.phone)
    print("\n")
  }

  //util

  def findAttr(url:String,xpath:String):List[String] = {
    try {
      val driver = new HtmlUnitDriver()
      driver.get(url)
      (driver.findElementsByXPath(xpath) map { i =>
        Option(i.getAttribute("href"))
      }).flatten.toList
    } catch {
      case e:Throwable => println("err1, " + url);List()
    }
  }

  def getText(url:String,xpath:String):Option[String] = {
    try {
      val driver = new HtmlUnitDriver()
      driver.get(url)
        (driver.findElementsByXPath(xpath) map { i =>
          i.getText()
        }).headOption
    } catch {
      case e:Throwable => println("err2, " + url);None
    }
  }

}
