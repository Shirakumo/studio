var Studio = function(){
    var self = this;
    
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

    self.constructElement = (tag, options)=>{
        var el = document.createElement(options.tag || tag);
        el.setAttribute("class", (options.classes||[]).join(" "));
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

    self.prompt = function(message, options){
        options = options || {};
        var prompt = self.constructElement("div", {
            classes: ["prompt", options.classes],
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

            image.addEventListener("drag", function(ev){
                var b = window.innerHeight / 20;
                if(ev.clientY < b){
                    window.scrollBy({top: -20, behaviour: "smooth"});
                }else if(window.innerHeight < ev.clientY + 2*b){
                    window.scrollBy({top: +20, behaviour: "smooth"});
                }
            });

            image.addEventListener("dragstart", function(ev){
                ev.dataTransfer.effectAllowed = "move";
                image.classList.add("move");
            });

            image.addEventListener("dragover", function(ev){
                ev.preventDefault();
                ev.dataTransfer.dropEffect = "move";
                var source = images.querySelector(".image.move");
                var target = ev.target;
                while(target && !target.classList.contains("image")) target = target.parentNode;
                if(source && target && target != source){
                    var g = target.getBoundingClientRect();
                    if(ev.clientY < g.top+(g.bottom-g.top)/2){
                        images.insertBefore(source, target);
                    }else{
                        images.insertBefore(source, target.nextSibling);
                    }
                }
            });

            image.addEventListener("dragend", function(ev){
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
            for(var i=0; i<fileSelect.files.length; i++){
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
                if(!title.checkValidity()) return false;
                if(files.length == 0) return false;
                // Gather form data
                var form = new FormData();
                form.append("data-format", "json");
                form.append("title", title.value);
                form.append("description", root.querySelector("[name=description]").value);
                form.append("tags", root.querySelector("[name=tags]").value);
                form.append("visibility", root.querySelector("[name=visibility]").value);
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
                    if(request.status == 200){
                        window.location = request.response["data"]["url"];
                    }else{
                        document.querySelector("#error").innerHTML = request.response["message"];
                    }
                };

                var submit = function(){
                    self.log("Submitting", action, form);
                    request.open("POST", ev.target.getAttribute("formaction"));
                    request.send(form);
                };
                
                if(action == "Delete") {
                    self.prompt("Are you sure you want to delete this?", {onYes: submit});
                } else {
                    submit();
                }
                return false;
            });
        });
    };

    var initSettings = function(root){
        self.log("Init settings", root);
    };

    var initGallery = function(root){
        self.log("Init gallery", root);

        // FIXME: implement prefetching
    };

    var initView = function(root){
        self.log("Init view", root);

        [].forEach.call(root.querySelectorAll(".image img"), function(img){
            img.addEventListener("click", function(){
                if(img.classList.contains("full")){
                    img.classList.remove("full");
                }else{
                    img.classList.add("full");
                }
            });
        });
    };

    self.init = function(root){
        var upload = root.querySelector(".upload");
        var gallery = root.querySelector(".gallery");
        var view = root.querySelector(".view");
        var settings = root.querySelector(".settings");
        
        if(upload) initUpload(upload);
        if(gallery) initGallery(gallery);
        if(view) initView(view);
        if(settings) initView(settings);
    };
};

var studio = null;
document.addEventListener("DOMContentLoaded", function(){
    studio = new Studio();
    studio.init(document);
});
