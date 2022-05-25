var Studio = function(){
    var self = this;
    
    self.options = (new URL(window.location)).searchParams;
    self.apiBase = document.querySelector("link[rel=api-base]");
    if(self.apiBase) self.apiBase = self.apiBase.getAttribute("href");
    self.nextPage = null;
    
    self.log = function(){
        if(console) console.log.apply(null, arguments);
    };

    self.find = function(thing, array){
        return 0 <= array.indexOf(thing);
    };

    self.remove = function(thing, array){
        var result = [];
        for(var el of array){
            if(el != thing) result.push(el);
        }
        return result;
    };

    self.addEventListeners = function(target, events, func){
        events.forEach(function(event){
            target.addEventListener(event, func);
        });
        return target;
    };

    self.equalp = function(a, b){
        if(a === null || b === null){
            return a === b;
        }
        
        var aF = Object.getOwnPropertyNames(a);
        var bF = Object.getOwnPropertyNames(b);
        if (aF.length != bF.length) {
            return false;
        }

        for (var i = 0; i < aF.length; i++) {
            var field = aF[i];
            if (a[field] !== b[field]) {
                return false;
            }
        }
        return true;
    };

    self.mergeInto = function(target, object){
        for(var key in object){
            target[key] = object[key];
        }
        return target;
    };

    self.show = function(element){
        var header = document.querySelector("#top>header").clientHeight;
        element.scrollIntoView({behaviour: "smooth"});
        element.focus();
    };

    self.constructElement = function(tag, options){
        var el = document.createElement(options.tag || tag);
        (options.classes||[]).forEach(function(clas){
            if(clas) el.classList.add(clas);
        });
        if(options.text) el.innerText = options.text;
        if(options.html) el.innerHTML = options.html;
        for(var attr in (options.attributes||{})){
            el.setAttribute(attr, options.attributes[attr]);
        }
        for(var tag in (options.elements||{})){
            var sub = self.constructElement(tag, options.elements[tag]);
            el.appendChild(sub);
        }
        return el;
    };

    self.extractPage = function(url){
        var path = decodeURI((new URL(url)).pathname);
        var matches = path.match("gallery/([^/]+)(?:/tag/(.+?))?(?:/([0-9]{1,2}\\.[0-9]{1,4})(?:\\+([0-9]+))?)?$");
        if(matches){
            return {
                user: matches[1],
                tag: matches[2],
                date: matches[3],
                offset: (matches[4])? parseInt(matches[4]) : 0
            };
        }else{
            var now = new Date();
            var author = document.querySelector("a[rel=author]");
            return {
                user: (author)?author.innerText:null,
                tag: null,
                date: (now.getMonth()+1)+"."+now.getFullYear(),
                offset: 0
            };
        }
    };

    self.pageUrl = function(page){
        var base = document.querySelector("a[rel=author]").getAttribute("href");
        base = base.substr(0, base.lastIndexOf("/"));
        return base+"/"+page.user
            +(page.tag?"/tag/"+page.tag:"")
            +"/"+page.date
            +(0<page.offset?"+"+page.offset:"");
    };

    self.currentPage = null;
    self.changeToPage = function(page){
        if(!self.equalp(page,self.currentPage)){
            self.log("Changing to page", page);
            if(!self.currentPage){
                window.history.pushState("prefetch", page.date+"+"+page.offset, self.pageUrl(page));
            }else{
                window.history.replaceState("prefetch", page.date+"+"+page.offset, self.pageUrl(page));
            }
            self.currentPage = page;
        }
        return self.currentPage;
    };

    self.imagesSectionPage = function(images){
        var date = images.querySelector("time").textContent;
        var page = self.extractPage(window.location.href);
        return self.mergeInto(page, {date: date, offset: 0});
    };

    var currentRequest = null;
    self.fetchUploads = function(page, onComplete){
        if(currentRequest) return currentRequest;
        self.log("Fetching uploads", page);
        var form = new FormData();
        form.append("data-format", "json");
        form.append("user", page.user);
        form.append("date", page.date);
        form.append("skip", page.offset);
        if(page.tag) form.append("tag", page.tag);
        var uploadRequest = new XMLHttpRequest();
        currentRequest = uploadRequest;
        uploadRequest.responseType = 'json';
        uploadRequest.onload = function(ev){
            currentRequest = null;
            if(uploadRequest.status == 200){
                onComplete(uploadRequest.response["data"]);
            }else{
                self.log("Failed to fetch uploads", ev, uploadRequest.response);
            }
        };
        uploadRequest.open("POST", self.apiBase+"upload/list");
        uploadRequest.send(form);
        return uploadRequest;
    };

    self.unixEpochDifference = 2208988800;
    self.uploadDate = function(upload){
        var date = new Date((upload.time-self.unixEpochDifference)*1000);
        return (date.getMonth()+1)+"."+date.getFullYear();
    };

    self.showUpload = function(upload){
        self.log(upload.visibility);
        var element = self.constructElement("article", {
            classes: ["image",
                      (1 < upload.files.length)? "multiple" : null,
                     upload.visibility],
            elements: { "a": {
                attributes: {"href": upload.url},
                elements: {"img": {
                    attributes: {"src": self.apiBase+"file?thumb=true&id="+upload.files[0]}
                }}
            }}
        });
        var date = self.uploadDate(upload);
        [].forEach.call(document.querySelectorAll(".images"), function(images){
            if(images.querySelector("time").innerText == date){
                images.appendChild(element);
                date = null;
            }
        });
        if(date){
            var images = self.constructElement("section", {
                classes: ["images"],
                elements: { "time": {text: date}}
            });
            images.appendChild(element);
            var last = document.querySelector(".images:last-child");
            last.parentElement.insertBefore(images, last.nextSibling);
        }
        return element;
    };

    self.isScrolledToBottom = function(){
        return (window.innerHeight + window.pageYOffset) >= document.body.offsetHeight - 2;
    };

    self.fetchNextPage = function(page){
        page = page || self.nextPage;
        if(!page) return null;
        self.nextPage = page;
        return self.fetchUploads(page, function(data){
            self.log("Fetched next page", data);
            data.uploads.forEach(self.showUpload);
            if(data.older){
                self.log("Found next page",data.older);
                self.mergeInto(self.nextPage, data.older);
                // Try again in case we didn't fetch enough to advance the bottom
                if(self.isScrolledToBottom()){
                    self.log("Still not at bottom, fetching more...");
                    self.fetchNextPage();
                }else{
                    self.log("No longer at bottom, stopping.");
                }
            }else{
                self.nextPage = null;
            }
        });
    };

    self.spin = function(options){
        options = options || {};
        var spinner = document.querySelector(".spinner");
        if(options.activate === undefined){
            options.activate = (spinner)? false : true;
        }
        if(options.activate && !spinner){
            spinner = self.constructElement("div", {
                classes: ["overlay", "spinner", options.classes],
                elements: {
                    "div": {
                        text: options.message || "Please Wait",
                        classes: ["container"],
                        elements: {"a": {tag:"div"},
                                   "b": {tag:"div"}}
                    }
                }
            });
            document.querySelector("body").appendChild(spinner);
        }else if(spinner){
            spinner.parentElement.removeChild(spinner);
        }
        return spinner;
    };

    self.prompt = function(options){
        options = options || {};
        var prompt = self.constructElement("div", {
            classes: ["overlay", "prompt", options.classes],
            elements: {
                "div": {
                    classes: ["container"],
                    elements: {
                        "i": {classes: ["far", "fa-question-circle"]},
                        "p": {text: options.message || "Are you sure?"},
                        "nav": {
                            elements: {
                                "a": {tag: "button", classes: ["yes"], text: options.yes || "Yes"},
                                "b": {tag: "button", classes: ["no"], text: options.no || "No"},
                            }
                        }
                    }
                }
            }
        });
        prompt.querySelector(".no").addEventListener("click", function(){
            if(options.onNo) options.onNo();
            prompt.parentElement.removeChild(prompt);
        });
        prompt.querySelector(".yes").addEventListener("click", function(){
            if(options.onYes) options.onYes();
            prompt.parentElement.removeChild(prompt);
        });
        (options.container || document.querySelector("body")).appendChild(prompt);
        return prompt;
    };

    var fileObjects = [];
    self.filePayload = function(root){
        root = root || document;
        var result = [];
        [].forEach.call(root.querySelectorAll(".image:not(.removed)"), function(image){
            if(image.dataset.file){
                result.push(fileObjects[parseInt(image.dataset.file)]);
            }else if(image.dataset.id){
                result.push(image.dataset.id);
            }else{
                log("Warning: image without id or file", image);
            }
        });
        return result;
    };
    
    var initUpload = function(root){
        self.log("Init upload", root);
        
        var fileSelect = root.querySelector(".new-image [type=file]");
        var images = root.querySelector(".images");
        
        var registerImage = function(image){
            image.querySelector(".remove").addEventListener("click", function(){
                if(image.classList.contains("removed")){
                    image.classList.remove("removed");
                    image.querySelector(".remove i").classList.remove("fa-undo");
                    image.querySelector(".remove i").classList.add("fa-trash-alt");
                }else{
                    image.classList.add("removed");
                    image.querySelector(".remove i").classList.add("fa-undo");
                    image.querySelector(".remove i").classList.remove("fa-trash-alt");
                }
                if(images.querySelectorAll(".image:not(.removed)").length == 0){
                    fileSelect.setAttribute("required", "required");
                }else{
                    fileSelect.removeAttribute("required");
                }
            });

            var handleDragOver = function(y, target){
                var source = images.querySelector(".image.move");
                while(target && target != document && !target.classList.contains("image"))
                    target = target.parentNode;
                if(source && target && target != document && target != source){
                    var g = target.getBoundingClientRect();
                    if(y < g.top+(g.bottom-g.top)/2){
                        images.insertBefore(source, target);
                    }else{
                        images.insertBefore(source, target.nextSibling);
                    }
                }
            };

            self.addEventListeners(image, ["drag","touchmove"], function(ev){
                var b = window.innerHeight / 20;
                if(ev.clientY < b){
                    window.scrollBy({top: -20, behaviour: "smooth"});
                }else if(window.innerHeight < ev.clientY + 2*b){
                    window.scrollBy({top: +20, behaviour: "smooth"});
                }
                if(ev instanceof TouchEvent){
                    var target = document.elementFromPoint(ev.touches.item(0).pageX, ev.touches.item(0).pageY);
                    if(target) handleDragOver(ev.touches.item(0).clientY, target);
                }
            });

            self.addEventListeners(image, ["dragstart","touchstart"], function(ev){
                if(ev instanceof DragEvent) ev.dataTransfer.effectAllowed = "move";
                image.classList.add("move");
            });

            image.addEventListener("dragover", function(ev){
                ev.preventDefault();
                ev.dataTransfer.dropEffect = "move";
                handleDragOver(ev.clientY, ev.target);
            });

            self.addEventListeners(image, ["dragend","touchend","touchcancel"], function(ev){
                ev.preventDefault();
                image.classList.remove("move");
            });
        };
        
        var showFile = function(file){
            var image = self.constructElement("div", {
                classes: ["image"],
                elements: {
                    "img": {},
                    "label": {
                        classes: ["remove"],
                        elements: { "i": {classes: ["fas", "fa-fw", "fa-trash-alt"]}}
                    }
                }
            });

            // Start loading image as soon as possible
            var reader = new FileReader();
            reader.onload = function(){ image.querySelector("img").src = reader.result; };
            reader.readAsDataURL(file);

            // Prepare the rest.
            image.dataset.file = fileObjects.length;
            fileObjects.push(file);
            registerImage(image);
            images.insertBefore(image, images.querySelector(".new-image"));
        };

        fileSelect.addEventListener("change", function(){
            fileSelect.removeAttribute("required");
            for(var i=fileSelect.files.length-1; i>=0; i--){
                showFile(fileSelect.files[i]);
            }
            fileSelect.value = '';
        });

        [].forEach.call(images.querySelectorAll(".image"), registerImage);

        [].forEach.call(root.querySelectorAll("[type=submit]"), function(element){
            element.addEventListener("click", function(ev){
                ev.preventDefault();
                var action = ev.target.value;
                var title = root.querySelector("[name=title]");
                var files = self.filePayload(images);
                // Check validity
                if(!title.checkValidity()){
                    self.show(title);
                    return false;
                }
                if(files.length == 0){
                    self.show(fileSelect);
                    return false;
                }
                // Gather form data
                var form = new FormData();
                form.append("data-format", "json");
                form.append("title", title.value);
                form.append("description", root.querySelector("[name=description]").value);
                form.append("tags", root.querySelector("[name=tags]").value);
                form.append("visibility", root.querySelector("[name=visibility]").value);
                form.append("license", root.querySelector("[name=license]").value);
                form.append("arrangement", root.querySelector("[name=arrangement]:checked").id);
                if(root.querySelector("[name=upload]")){
                    form.append("upload", root.querySelector("[name=upload]").value);
                }
                files.forEach(function(file){
                    form.append("file[]", file);
                });
                // Submit form via AJAX
                var request = new XMLHttpRequest();
                request.responseType = 'json';
                request.onload = function(ev){
                    self.log("Submission complete", request);
                    self.spin({activate: false});
                    if(request.status == 200){
                        var url = request.response["data"]["url"];
                        window.location = url || document.querySelector("[rel=author]").getAttribute("href");
                    }else{
                        document.querySelector("#error").innerHTML = request.response["message"];
                    }
                };

                var submit = function(){
                    self.log("Submitting", action, form);
                    self.spin({activate: true});
                    request.open("POST", ev.target.getAttribute("formaction"));
                    request.send(form);
                };
                
                if(action == "Delete") {
                    self.prompt({
                        message: "Are you sure you want to delete this?",
                        onYes: submit
                    });
                } else {
                    submit();
                }
                return false;
            });
        });
    };

    var initFront = function(root){
        self.log("Init front", root);
    };

    var initSettings = function(root){
        self.log("Init settings", root);

        var forceDelete = false;
        var deleteButton = root.querySelector("input[value=Delete]");
        if(deleteButton){
            deleteButton.addEventListener("click", function(ev){
                if(!forceDelete){
                    ev.preventDefault();
                    self.prompt({
                        message: "Are you sure you want to delete your gallery and all of your uploads?",
                        onYes: function(){
                            forceDelete = true;
                            root.querySelector("input[value=Delete]").click();
                        }
                    });
                    return false;
                }
                return true;
            });
        }
    };

    var initGallery = function(root){
        self.log("Init gallery", root);

        var next = document.querySelector(".navlink.next");
        if(next){
            next.parentElement.removeChild(next);
            var nextPage = self.extractPage(next.getAttribute("href"));
            window.addEventListener("scroll", function(ev){
                if(self.isScrolledToBottom()){ self.fetchNextPage(); }
                var largest = null;
                [].forEach.call(root.querySelectorAll(".images"), function(images){
                    var offset = images.getBoundingClientRect().top;
                    if(!largest
                       || (offset < 200 && largest.getBoundingClientRect().top < offset)
                       || self.isScrolledToBottom()){
                        largest = images;
                    }
                });
                self.changeToPage(self.imagesSectionPage(largest));
            });
            self.fetchNextPage(nextPage);
        }
    };

    var initView = function(root){
        self.log("Init view", root);

        var readingOptions = (self.options.get("reading-options") || "").split(",");
        [].forEach.call(root.querySelectorAll("input.reading-option"), function(input){
            if(self.find(input.getAttribute("id"), readingOptions)){
                input.checked = true;
            }
        });
    };

    self.init = function(root){
        var front = root.querySelector("article.studio>.front");
        var upload = root.querySelector("article.studio>.upload");
        var gallery = root.querySelector("article.studio>.gallery");
        var view = root.querySelector("article.studio>.view");
        var settings = root.querySelector("article.studio>.settings");

        if(front) initFront(front);
        if(upload) initUpload(upload);
        if(gallery) initGallery(gallery);
        if(view) initView(view);
        if(settings) initSettings(settings);
    };
};

var studio = null;
document.addEventListener("DOMContentLoaded", function(){
    studio = new Studio();
    studio.init(document);
});
