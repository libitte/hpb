package jp.mery
import scala.util.Try
import scala.collection.JavaConversions._
import org.openqa.selenium.htmlunit.HtmlUnitDriver

object App {

  def main(args: Array[String]){
    val area = List(
      //"http://beauty.hotpepper.jp/svcSA/macAD/salon/",//ヘア 渋谷
      //"http://beauty.hotpepper.jp/svcSA/macAA/salon/",//ヘア 新宿
      //"http://beauty.hotpepper.jp/svcSA/macAB/salon/" //ヘア 池袋
      //"http://beauty.hotpepper.jp/svcSA/macAC/salon/sacX011/" //ヘア 六本木・麻布・赤坂

      "http://beauty.hotpepper.jp/nail/svcSA/macAB/salon/" // ネイル 池袋・目白
      //"http://beauty.hotpepper.jp/nail/svcSA/macAC/salon/" // ネイル 恵比寿・代官山・中目黒・広尾・麻布・六本木
      //"http://beauty.hotpepper.jp/nail/svcSA/macAA/salon/" // ネイル 新宿・高田馬場・代々木
      //"http://beauty.hotpepper.jp/nail/svcSA/macAD/salon/" // ネイル 渋谷・青山・表参道・原宿
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
    //val name = getText(url,"//em[@class='seoCaption']")
    val name = getText(url,"//div[@id='mainContents']//p[@class='detailTitle']//a")
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
