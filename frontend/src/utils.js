function preloadImages(urls, callback) {
  var images = {};
  var loadedImages = 0;
  var imgCount = urls.length;

  if (imgCount == 0)
    callback([]);
  
  for (var url in urls) {
    images[url] = new Image();
    images[url].onload = function() {
      if (++loadedImages >= imgCount) {
        callback(images);
      }
    }
    images[url].src = url;
  }
}

function preloadImage(url, callback) {
  var img = new Image();
  img.onload = function() {
    callback(img);
  }
  img.src = url;
}
